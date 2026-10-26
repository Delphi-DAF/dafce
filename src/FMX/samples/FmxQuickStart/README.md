# FmxQuickStart — el arranque de la familia dentro de una app FireMonkey

Un formulario con un campo, un botón y una etiqueta, y un servicio que llega al formulario por
inyección de propiedad. Lo mínimo para ver dónde encaja DAF en una aplicación de escritorio FMX —
y dónde deja de mandar.

Su equivalente en VCL, mucho más grande, es `VCL/samples/TicTacToeApp`.

## Qué enseña

- **El arranque es el mismo que el de las demás variantes hasta que la plataforma se queda con el
  bucle.** `TFmxApplication.CreateBuilder` da el mismo componedor —servicios, configuración,
  entorno y logging— que `TCliApplication` o `TWebApplication`. Lo que cambia es el final:
  `Run(TMainForm)` crea el formulario principal y **delega** en `FMX.Forms.Application`, que es
  quien tiene el *message loop*. En CLI y en Web, `Run` **es** el proceso y el `.dpr` acaba cuando
  `Run` acaba. Son dos contratos de ciclo de vida, no dos variaciones de uno.
- **`Run` toma una clase de formulario y no un parámetro de tipo.** Una interfaz de Delphi no
  admite métodos genéricos, y `IFmxApplication` es una interfaz.
- **El formulario lo construye FireMonkey, así que no hay constructor que el contenedor pueda
  elegir.** La vía es la inyección de propiedad: `[DafInject]` sobre `Greeter`, y
  `TActivatorUtilities.InjectInto(DafApp.Services, Self)` en el `FormCreate`. Es el caso para el
  que existe la inyección por propiedad, y `Microsoft.Extensions.DependencyInjection` no la tiene.
- **`DafApp` se llama así y no `Application` porque el choque no es hipotético**: en este mismo
  `.dpr` conviven el global de DAF y el de FireMonkey. La vía preferente sigue siendo la
  inyección; `DafApp` es para el código que la plataforma construye y al que nadie le puede
  inyectar nada.
- **`Daf.FMX` es el único paquete de DAF que enlaza FireMonkey**, y no es soporte de plataforma:
  los objetivos declarados siguen siendo Win32 y Win64.

## Ejecutar

Es una aplicación de escritorio: hay que lanzarla en una sesión con interfaz gráfica.

```
BDS make --project:src/FMX/samples/FmxQuickStart/FmxQuickStart.dproj
out\Win32\samples\bin\FmxQuickStart.exe
```

Escribir un nombre y pulsar **Saludar**: la etiqueta de abajo muestra el saludo y cuántos lleva.
El contador sube porque `IGreeter` está registrado como **singleton** — con `AddTransient` volvería
a 1 en cada pulsación.

⚠️ **No se puede ejercitar sin escritorio**, y conviene saberlo antes de meterlo en un script:
FireMonkey no crea su `Application` global en un proceso sin contexto gráfico, así que crear el
formulario muere ahí. Es la misma razón por la que la feature del módulo llega solo hasta el borde
del bucle, con `StartUpTo`.
