# Commons — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Daf.MemUtils — Punteros inteligentes](#dafmemutils--punteros-inteligentes)
2. [Daf.Threading — Tokens de cancelación y apagado](#dafthreading--tokens-de-cancelación-y-apagado)
3. [Daf.Enumerable](#dafenumerable)
4. [Daf.Rtti](#dafrtti)
5. [Daf.Activator](#dafactivator)
6. [Daf.Arrays](#dafarrays)

`Daf.SystemProcess` y `Daf.CmdLn.Parser` se documentaban aquí. La primera vive en un paquete propio
—ver [SystemProcess](../../SystemProcess/README.es.md)—; la segunda se ha retirado, y la línea de
comandos es [`Daf.CommandLine`](../../CommandLine/README.es.md).

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

### TDeferredEnumerable\<T\>

`IEnumerable<T>` construido a partir de un closure que devuelve el enumerador — la primitiva de
secuencia diferida. No se ejecuta nada hasta que se llama a `GetEnumerator`, y cada iteración vuelve
a invocar la factoría, así que la misma secuencia se puede recorrer más de una vez.

```pascal
uses Daf.Enumerable;

var Llamadas := 0;
var Seq: IEnumerable<Integer> := TDeferredEnumerable<Integer>.Create(
  function: IEnumerator<Integer>
  var
    N: Integer;
  begin
    Inc(Llamadas);
    N := 0;
    Result := TGeneratorEnumerator<Integer>.Create(
      function(var Current: Integer): Boolean
      begin
        Result := N < 3;
        if Result then
        begin
          Inc(N);
          Current := N;
        end;
      end);
  end);

// Llamadas sigue en 0: declarar la secuencia no enumeró nada

for var I in Seq do    // Llamadas = 1, entrega 1, 2, 3
  Procesar(I);

for var I in Seq do    // Llamadas = 2 — un enumerador nuevo, recorrido desde el principio
  Procesar(I);
```

Es la primitiva sobre la que está construido `Daf.Linq`: `Where`, `Select`, `Take` y los demás
devuelven uno de estos envolviendo la secuencia anterior, y eso es lo que hace que una cadena de
operadores recorra la fuente una sola vez, y solo cuando un terminal pide un elemento. Ver
[Daf.Linq](../../Linq/README.md).

### TGeneratorEnumerator\<T\>

El enumerador de la pareja anterior, construido con un closure de avanzar y entregar: un
`TGeneratorFunc<T>` es una `function(var Current: T): Boolean` que devuelve `False` cuando la
secuencia se agotó y, si no, escribe el elemento siguiente en `Current`. El estado del recorrido
vive en el closure, así que una secuencia sin colección detrás — un contador, un fichero que se
lee, una serie calculada — no necesita una clase enumeradora propia.

`Reset` lanza `ENotImplemented`: un closure que ya avanzó no se puede rebobinar. Recorrer una
secuencia por segunda vez es para lo que está `TDeferredEnumerable<T>` — le pide a su factoría un
enumerador **nuevo**, con su propio estado.

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
`Where` / `Any` toman un `TFilterSpec<T>` y `Select` un `TFunc<T, TTarget>` — los
mismos tipos que toman los operadores perezosos de `Daf.Linq`,
así que un filtro o un mapper escrito una vez sirve para las dos APIs. Los closures
en línea convierten implícitamente, como arriba; construye el valor por separado
cuando quieras reutilizarlo o componerlo:

```pascal
uses Daf.FilterSpec;

var Largos: TFilterSpec<string> := TPredicate<string>(
  function(S: string): Boolean begin Result := Length(S) > 4; end);
var Gritos: TFilterSpec<string> := TPredicate<string>(
  function(S: string): Boolean begin Result := S = S.ToUpper; end);

Items.Where(Largos and not Gritos);      // compuesto con and / or / not
```

