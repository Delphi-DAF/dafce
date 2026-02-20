# Commons — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Daf.MemUtils — Punteros inteligentes](#dafmemutils--punteros-inteligentes)
2. [Daf.Threading — Cancelación y futuros](#dafthreading--cancelación-y-futuros)
3. [Daf.Enumerable](#dafenumerable)
4. [Daf.SystemProcess](#dafsystemprocess)
5. [Daf.CmdLn.Parser](#dafcmdlnparser)
6. [Daf.Rtti](#dafrtti)
7. [Daf.Activator](#dafactivator)
8. [Daf.Arrays](#dafarrays)

---

## Daf.MemUtils — Punteros inteligentes

### ARC\<T\>

Envoltorio de conteo automático de referencias. Cuando la última variable `ARC<T>` que contiene un objeto sale del scope, el objeto es liberado.

```pascal
uses Daf.MemUtils;

// Crear
var Doc := ARC<TDocument>.Create(TDocument.Create);

// Acceder al objeto envuelto
Doc.Value.Load('informe.pdf');

// Pasar entre variables — el conteo de referencias aumenta, se libera cuando la última copia sale del scope
var Copia := Doc;
```

**Reglas clave**

- `ARC<T>` es un record — la asignación copia el handle e incrementa el conteo.
- `Value` lanza una excepción si el objeto interno es nil.
- Soporta interfaces mediante `ARC<IMyInterface>`.

### WRef\<T\>

Referencia débil — **no** impide que el objeto envuelto sea liberado.

```pascal
var Weak: WRef<TMyObject>;
Weak := WRef<TMyObject>.Create(RefFuerte);

if Weak.IsAlive then
  Weak.Value.DoWork;
```

---

## Daf.Threading — Cancelación y futuros

### ICancellationToken / ICancellationTokenSource

```pascal
uses Daf.Threading;

var Cts   := TCancellationTokenSource.Create;
var Token := Cts.Token;          // ICancellationToken

// En el worker:
while not Token.IsCancellationRequested do
  ProcesarSiguienteElemento;

// Llamador:
Cts.Cancel;
```

### IFuture\<T\>

Ejecuta trabajo en un hilo en segundo plano y recupera el resultado más tarde.

```pascal
var Future := TFuture<string>.Run(
  function: string
  begin
    Result := ObtenerDeAPI;
  end);

// … hacer otro trabajo …

var Datos := Future.Value;   // bloquea hasta que se complete
```

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

### IEnumerable\<T\>

Secuencia lazy de solo avance sobre cualquier fuente de datos.

```pascal
uses Daf.Enumerable;

var Seq := TListEnumerable<integer>.Create(MiLista);
for var Item in Seq do
  Procesar(Item);
```

### IInterfaceList\<T\>

Lista con tipo seguro de referencias a interfaces.

```pascal
var Lista: IInterfaceList<IMyService>;
Lista := TInterfaceList<IMyService>.Create;
Lista.Add(ServicioA);
Lista.Add(ServicioB);

for var Svc in Lista do
  Svc.Execute;
```

### TOrderedDictionary

Diccionario que preserva el orden de inserción al iterar.

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

Parsea los argumentos de línea de comandos en switches nombrados y valores posicionales.

```pascal
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParser.Create;
Parser.Parse(ParamStr(0));

var Verbose := Parser.Flag('v');           // --v o -v
var Port    := Parser.Value('port', '80'); // --port 8080 (por defecto '80')
var Files   := Parser.Positionals;         // argumentos restantes
```

---

## Daf.Rtti

Helpers para el Extended RTTI de Delphi.

```pascal
uses Daf.Rtti;

// Comprobar herencia en tiempo de ejecución
if _T.Extends(AlgunaClase, TMyBaseClass) then
  WriteLn('Es derivada');
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

Helpers de extensión de `TArray`.

```pascal
uses Daf.Arrays;

var Idx := TArrayHelper.IndexOf<string>(MiArray, 'aguja');
if Idx >= 0 then
  WriteLn('Encontrado en ', Idx);
```
