# Commons — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Daf.MemUtils — Punteros inteligentes](#dafmemutils--punteros-inteligentes)
2. [Daf.Threading — Tokens de cancelación y apagado](#dafthreading--tokens-de-cancelación-y-apagado)
3. [Daf.Enumerable](#dafenumerable)
4. [Daf.SystemProcess](#dafsystemprocess)
5. [Daf.CmdLn.Parser](#dafcmdlnparser)
6. [Daf.Rtti](#dafrtti)
7. [Daf.Activator](#dafactivator)
8. [Daf.Arrays](#dafarrays)

---

## Daf.MemUtils — Punteros inteligentes

### ARC\<T\>

`ARC<T>` es un «smart pointer» implementado como `reference to function: T`. Al invocarlo como función obtienes el objeto; cuando todas las referencias desaparecen del scope el objeto se libera automáticamente.

```pascal
uses Daf.MemUtils;

// Crear desde una instancia existente
var Doc := ARC.From<TDocument>(TDocument.Create);

// Crear + instanciar automáticamente (requiere constructor sin argumentos)
var Doc2 := ARC.From<TDocument>;

// Acceder al objeto — se invoca como función
Doc().Load('informe.pdf');
WriteLn(Doc().Title);

// Pasar entre variables — el refcount aumenta; se libera al salir del scope
var Copia := Doc;
```

**Reglas clave**

- `ARC<T>` es un `reference to function: T` (cierre de interfaz), no un record.
- Se accede al objeto invocando la variable como función: `MyArc()`, no `.Value`.
- `ARC.From<T>(nil)` devuelve `nil`.

---

## Daf.Threading — Tokens de cancelación y apagado

### ICancellationToken / ICancellationTokenSource

Usar las funciones de fábrica para crear un origen de cancelación — `TCancellationTokenSource` es una clase de implementación interna y no se instancia directamente.

```pascal
uses Daf.Threading;

// Crear mediante funciones de fábrica
var Cts := CreateCancellationTokenSource;           // estándar
// var Cts := CreateCancellationTokenSourceWithTimeout(5000); // auto-cancela tras 5 s
// var Cts := CreateCanceledCancellationTokenSource;          // ya cancelado

var Token := Cts.Token;  // ICancellationToken

// En el worker:
while not Token.IsCancellationRequested do
  ProcesarSiguienteElemento;

// Notificar cuando se cancela:
Token.Register(procedure begin WriteLn('Cancelado!'); end);

// Cancelar desde el llamador:
Cts.Cancel;
```

> **Nota:** `IFuture<T>` forma parte del RTL de Delphi (`System.Threading`), no de DAF.
> Usa `TTask.Future<T>` (RTL) para cálculos en segundo plano, o `TSystemProcess.ExecuteAsync`
> (DAF) para ejecución de procesos externos.

### TShutdownHook

Conecta las señales de apagado del OS (Ctrl+C, SIGTERM) a la limpieza de la aplicación:

```pascal
uses Daf.Threading;

TShutdownHook.OnShutdownRequested := procedure
begin
  Logger.LogInformation('Apagado solicitado, deteniendo…', []);
  App.Stop;
end;
```

---

## Daf.Enumerable

### ToIEnumerable\<T\>

Record-puente que adapta cualquier `TEnumerable<T>` (clase) a la interfaz `IEnumerable<T>`. Los operadores de conversión implícita hacen el uso transparente.

```pascal
uses Daf.Enumerable;

var Lista := TList<Integer>.Create;
Lista.Add(1); Lista.Add(2); Lista.Add(3);

// Conversión implícita a IEnumerable<T>
var Seq: IEnumerable<Integer> := ToIEnumerable<Integer>(Lista);
for var Item in Seq do
  Procesar(Item);

// Convertir a array plano
var Arr := ToIEnumerable<Integer>(Lista).ToArray;
```

### IInterfaceList\<T\>

Lista con tipo seguro de referencias a interfaces, con soporte `GetEnumerator`.

```pascal
var Lista := TInterfaceList<IMyService>.Create;
Lista.Add(ServicioA);
Lista.Add(ServicioB);

if not Lista.IsEmpty then
  for var Svc in Lista do
    Svc.Execute;

// Buscar por predicado
var Encontrado: IMyService;
if Lista.TryFind(
    function(I: IInterface): Boolean
    begin Result := (I as IMyService).Name = 'objetivo'; end,
    Encontrado) then
  Encontrado.Execute;
```

---

## Daf.SystemProcess

Ejecuta procesos externos de forma asíncrona con callbacks de eventos.

```pascal
uses Daf.SystemProcess;

var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['log', '--oneline', '-10'])
  .WorkingDir('C:\MiRepo')
  .Timeout(30000)               // ms, INFINITE = 0
  .KillAfterTimeout(True)
  .HideWindow(True)
  .OnStdOut(procedure(Line: string) begin WriteLn(Line); end)
  .OnStdErr(procedure(Line: string) begin WriteLn('ERR: ', Line); end)
  .OnCompleted(procedure(R: TProcessResult)
    begin
      WriteLn('Exit=', R.ExitCode, ' duración=', R.Duration.TotalMilliseconds, 'ms');
    end)
  .OnFailed(procedure(R: TProcessResult)
    begin
      WriteLn('Fallido: ', R.LastError);
    end)
  .Build;

// Síncrono
var Resultado := Proc.Execute;

// O asíncrono
var Future := Proc.ExecuteAsync;
```

### TProcessResult

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `Status` | `TStatus` | `Completed`, `Failed`, `Timeout`, `Canceled`, `Killed` |
| `ExitCode` | `DWORD` | Código de salida del proceso |
| `Duration` | `TTimeSpan` | Tiempo de ejecución de reloj de pared |
| `LastError` | `string` | Descripción del error (casos de fallo/kill) |
| `Succeeded` | `function: Boolean` | `Status = Completed` y `ExitCode = 0` |

### Integración con TShutdownHook

```pascal
TShutdownHook.OnShutdownRequested := procedure
begin
  Proc.Kill;
end;
```

---

## Daf.CmdLn.Parser

Construye una gramática de argumentos tipados con `TCmdLnParserBuilder` y luego parsea la línea de comandos en `ICmdLParams`.

```pascal
uses Daf.CmdLn.Parser;

// 1. Definir la gramática
var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')      // argumento string con nombre
  .Arg<Integer>('port')     // argumento integer con nombre
  .Arg<Boolean>('verbose')  // flag booleano
  .Build;                   // devuelve ICmdLnParser

// 2. Parsear la línea de comandos real
var Params := Parser.Parse(GetCommandLine);

// 3. Leer valores (ICmdLParams.Item[] es la propiedad default)
var Archivo   := Params['file'].AsString;
var Puerto    := Params['port'].AsInteger;
var EsVerbose := Params['verbose'].AsBoolean;
```

### Subcomandos

```pascal
var Parser := TCmdLnParserBuilder.Create
  .Command('build')
    .Arg<string>('output')
  .EndCommand
  .Command('run')
    .Arg<Boolean>('watch')
  .EndCommand
  .Build;

var Params := Parser.Parse(GetCommandLine);
var Cmd := Params.Command;   // 'build' o 'run'
if Cmd = 'build' then
begin
  var Seccion := Params.GetSection('build');
  var DirSal  := Seccion['output'].AsString;
end;
```

---

## Daf.Rtti

Helpers para el Extended RTTI de Delphi, centralizados en la clase utilitaria `_T`.

```pascal
uses Daf.Rtti;

// Nombres de tipo y GUIDs
var Nombre := _T.NameOf<IMyService>;          // 'IMyService'
var Guid   := _T.GUID<IMyService>;            // TGUID
var Unidad := _T.UnitNameOf<IMyService>;      // 'MiUnidad'

// Comprobar jerarquía de interfaces (sobrecargas con PTypeInfo)
if _T.Extends(TypeInfo(IServicioHijo), TypeInfo(IMyService)) then
  WriteLn('IServicioHijo extiende IMyService');

// Comprobar si una clase tiene una propiedad
if _T.HasProperty<TMiClase>('Nombre') then
  WriteLn('La propiedad Nombre existe');

// Crear valor por defecto (cero) para cualquier tipo
var Cero := _T.Default<Integer>;             // 0
```

### TRttiTypeHelper / TRttiPackageHelper

```pascal
var RC := TRttiContext.Create;
var T  := RC.GetType(TypeInfo(TMiClase));

if T.IsCollection then WriteLn('Es una colección');
if T.Implements<IMyService> then WriteLn('Implementa IMyService');

// Descubrir todas las implementaciones de una interfaz en un paquete
RC.FindType('MiUnidad.TMiClase').AsInstance.DeclaringUnitPackage
  .DiscoverImpl<IMyService>(False, nil,
    procedure(Intf: TRttiInterfaceType; Impl: TRttiInstanceType)
    begin WriteLn(Impl.Name); end);
```

---

## Daf.Activator

Crea instancias de clases por referencia de clase o typeinfo mediante RTTI.

```pascal
uses Daf.Activator;

var Instancia := TActivator.CreateInstance(TMyServiceClass) as IMyService;
```

---

## Daf.Arrays

### TArrayHelper

Class helper para el `TArray` estándar — añade `Map` (proyección) y `Trim` (elimina espacios / cadenas vacías).

```pascal
uses Daf.Arrays;

// Map: proyectar [string] -> [Integer]
var Longitudes := TArray.Map<string, Integer>(Nombres,
  function(S: string): Integer begin Result := S.Length; end);

// Trim: eliminar espacios y elementos vacíos de un array de strings
var Limpio := TArray.Trim(['  hola  ', '', '  mundo  ']);
// Limpio = ['hola', 'mundo']
```

### TSmartArray\<T\>

Wrapper de tipo valor para `TArray<T>` con API rica estilo LINQ. Soporta operadores `in`, `+`, `=`, conversión implícita a/desde `TArray<T>`, y conversión implícita desde string CSV.

```pascal
var Items: TSmartArray<string>;
Items.Concat('alfa');
Items.Concat('beta');
Items.Concat('gamma');

// Membresía
if 'beta' in Items then WriteLn('Encontrado');

// Filtrar
var Largos := Items.Where(
  function(S: string): Boolean begin Result := Length(S) > 4; end);

// Proyectar
var Mayus := Items.Select<string>(
  function(S: string): string begin Result := S.ToUpper; end);

// Concatenación mediante operador +
var Más: TSmartArray<string> := Items + ['delta', 'epsilon'];

// Ordenar
Items.Sort;
```
