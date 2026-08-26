# Configuration — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Construir una configuración](#construir-una-configuración)
2. [Leer valores](#leer-valores)
3. [Distinguir una clave ausente de una vacía](#distinguir-una-clave-ausente-de-una-vacía)
4. [Claves jerárquicas y secciones](#claves-jerárquicas-y-secciones)
5. [Proveedor JSON](#proveedor-json)
6. [Proveedor INI](#proveedor-ini)
7. [Proveedor de variables de entorno](#proveedor-de-variables-de-entorno)
8. [Proveedor en memoria](#proveedor-en-memoria)
9. [Proveedor encadenado](#proveedor-encadenado)
10. [Orden de capas y sobreescritura](#orden-de-capas-y-sobreescritura)
11. [Binding a objetos](#binding-a-objetos)
12. [Proveedores personalizados](#proveedores-personalizados)

---

## Construir una configuración

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.Env,
  Daf.Extensions.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

var Config: IConfiguration := Builder.Build;
```

⚠️ **Sostén el builder en una variable `IConfigurationBuilder`.** `TConfigurationBuilder` lleva conteo
de referencias, así que encadenar directamente sobre `TConfigurationBuilder.Create` deja el builder sin
dueño y se pierde.

`TConfigurationBuilder.Build` devuelve un `IConfigurationRoot`, que también implementa `IConfiguration`.

---

## Leer valores

```pascal
// Acceso directo por clave (devuelve '' si no se encuentra)
var Value := Config['MiClave'];

// Clave jerárquica
var Conn := Config['Database:ConnectionString'];

// Sección y luego clave
var Port := Config.GetSection('Database')['Port'];

// Iterar hijos de una sección
for var Child in Config.GetSection('Logging').GetChildren do
  WriteLn(Child.Key, ' = ', Child.Value);
```

Las rutas de clave usan `:` como separador de jerarquía. Los arrays usan sufijos numéricos desde cero: `Plugins:0`, `Plugins:1`, etc.

---

## Distinguir una clave ausente de una vacía

`IConfiguration` responde a la presencia con dos miembros que .NET no tiene, y conviene saber por
qué: **allí la distinción la lleva la anulabilidad** — el indexador es `string? this[key]`, con
`null` para ausente y `''` para presente y vacía. Una cadena de Delphi no puede ser nula, así que
lo que allí hace el `?` aquí necesita un miembro propio.

```pascal
if Config.Exists('Features:Beta') then ;              // la presencia, y nada más
var Read := Config.Find('Features:Beta');             // presencia Y valor, una sola lectura
if Read.HasValue and (Read.Value = 'on') then ;
var Value := Config['Features:Beta'];                 // '' si no está Y si está vacía
```

`Exists` es paridad literal con el `IConfigurationSection.Exists()` de .NET, **semántica de sección
incluida**: una sección existe cuando tiene valor propio **o** tiene claves por debajo. Una sección
que solo agrupa —la que contiene `Logging:Level` y hermanas— no tiene valor propio, y contestar que
no existe mientras se le leen los hijos es una respuesta falsa sobre la forma del documento.

**El indexador sigue devolviendo `string` a propósito**, y eso está medido, no supuesto. Un retorno
anulable convierte de forma implícita, y esa conversión se aplica en la comparación, así que
`C['ausente'] = ''` y `C['vacía'] = ''` siguen dando las dos `True`. No arreglaría el defecto: lo
escondería tras un tipo que promete arreglarlo, que es peor que una cadena que no promete nada.
Que `Find` haya que nombrarlo es lo que cierra ese agujero: la presencia solo se puede leer
preguntándosela al tipo.

Esto muerde sobre todo mientras la configuración se está **componiendo**, porque una clave leída
antes de añadir la fuente que la trae se lee exactamente igual que una clave con valor vacío —
compila, se ejecuta, no lanza nada, y la funcionalidad no se activa jamás.

---

## Claves jerárquicas y secciones

Dado este JSON:

```json
{
  "Database": {
    "Host": "localhost",
    "Port": 5432,
    "Name": "mydb"
  },
  "Logging": {
    "Level": "Information"
  }
}
```

```pascal
Config['Database:Host']                // 'localhost'
Config['Database:Port']                // '5432'
Config.GetSection('Database')['Name']  // 'mydb'

var Db := Config.GetSection('Database');
Db['Host']      // 'localhost'
Db.HasChildren  // True
Db.Key          // 'Database'
Db.Path         // 'Database'
```

---

## Proveedor JSON

```pascal
uses Daf.Configuration.Json;

JsonConfig.AddFile(Builder, 'appsettings.json');                           // requerido
JsonConfig.AddFile(Builder, 'appsettings.Development.json', [csoOptional]); // opcional
```

Soporta objetos anidados y arrays. Las claves se aplanan con `:`.

---

## Proveedor INI

```pascal
uses Daf.Configuration.Ini;

IniConfig.AddFile(Builder, 'config.ini');
```

Formato INI:
```ini
[Database]
Host=localhost
Port=5432
```

Se accede como `Config['Database:Host']`.

---

## Proveedor de variables de entorno

```pascal
uses Daf.Configuration.Env;

Builder.Add(TEnvironmentVariablesConfigurationSource.Create);           // todas las variables
Builder.Add(TEnvironmentVariablesConfigurationSource.Create('MYAPP_'));  // por prefijo (se elimina)
```

`Daf.Configuration.Env` declara además `AddEnvironmentVariables` como class helper de
`TConfigurationBuilder`. Solo aplica si el builder se sostiene como **clase**, no como
`IConfigurationBuilder` — y sostenerlo como clase es justo lo que lo pierde, así que usa la forma de
arriba.

Con el prefijo `MYAPP_`, la variable `MYAPP_DATABASE__HOST` se convierte en `Database:Host` (doble `__` → `:`).

---

## Proveedor en memoria

Útil para tests o bootstrap:

```pascal
uses Daf.Configuration.Memory;

var Data := TDictionary<string, string>.Create;   // la fuente se queda con la propiedad
Data.Add('Database:Host', 'localhost');
Data.Add('Database:Port', '5432');

MemoryConfig.AddCollection(Builder, Data);
```

---

## Proveedor encadenado

Envuelve un `IConfiguration` **que ya existe** como fuente de otro builder. Es la única vía para
sembrar un builder desde una configuración que te entregaron: la fuente en memoria toma un
diccionario de cadenas, no un `IConfiguration`.

```pascal
uses Daf.Configuration.Chained;

// Una configuración que resolvió otro y te dio -- un anfitrión de plugins, un arnés de specs,
// una librería que compone sus propios valores por defecto.
var Given: IConfiguration := HostedBy.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
AddChainedConfiguration(Builder, Given);              // lo que te dieron, primero
JsonConfig.AddFile(Builder, 'appsettings.json');      // lo tuyo, encima

var Config := Builder.Build;
```

**La precedencia la decide quien compone**, no la fuente encadenada: no se arroga ninguna propia,
así que gana o pierde solo por dónde se la añadió. Y encadenar una configuración no asignada falla
en el acto en vez de en silencio — es una decisión del que compone, no un dato de entrada.

Una clave cuyo valor es la **cadena vacía** sobrevive a la cadena: la fuente informa de la
presencia preguntándosela a la configuración envuelta, y no infiriéndola de que el valor no esté
vacío. Esa distinción es la que repuso la
[consulta de presencia](#distinguir-una-clave-ausente-de-una-vacía).

> Este proveedor ya no tiene llamador dentro del framework, y eso es un cambio de papel, no de
> comportamiento. Era la costura que unía la configuración de host con la de aplicación; con un
> solo estilo de builder hay una sola configuración y nada que unir. Se queda porque no tiene
> sustituto, y .NET lo mantiene público por lo mismo.

## Orden de capas y sobreescritura

Los proveedores se aplican **en orden de registro**. Un valor de un proveedor posterior sobreescribe la misma clave de uno anterior:

```pascal
JsonConfig.AddFile(Builder, 'appsettings.json');                            // valores base
JsonConfig.AddFile(Builder, 'appsettings.Development.json', [csoOptional]); // entorno
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);               // el SO sobreescribe
CommandLineConfig.AddArgs(Builder);                                         // y la línea de comandos gana
```

Para recargar todos los proveedores (p. ej., tras un cambio de fichero):

```pascal
(Config as IConfigurationRoot).Reload;
```

---

## Binding a objetos

`TConfigurationBinder` mapea las claves de una sección a los campos públicos de una clase mediante RTTI:

```pascal
uses Daf.Configuration.Binder;

type
  TDatabaseOptions = class
  public
    Host: string;
    Port: Integer;
    Name: string;
    MaxPoolSize: Integer;
  end;

var Opts := TDatabaseOptions.Create;
TConfigurationBinder.Bind(Config.GetSection('Database'), Opts);

WriteLn(Opts.Host);         // 'localhost'
WriteLn(Opts.Port);         // 5432
WriteLn(Opts.MaxPoolSize);  // 0 si no está en la config
```

La conversión de tipos desde `string` se gestiona automáticamente para `Integer`, `Boolean`, `Double` y `string`.

---

## Proveedores personalizados

Implementa `IConfigurationSource` e `IConfigurationProvider`:

```pascal
type
  TVaultSource = class(TInterfacedObject, IConfigurationSource)
  public
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  TVaultProvider = class(TInterfacedObject, IConfigurationProvider)
  public
    function TryGet(const Key: string; out Value: string): Boolean;
    procedure &Set(const Key: string; const Value: string);
    procedure Load;
    function GetChildKeys(const EarlierKeys: IEnumerable<string>;
      const ParentPath: string): IEnumerable<string>;
  end;

function TVaultSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TVaultProvider.Create;
end;

// Registrar
Builder.Add(TVaultSource.Create);
```
