# Daf.VCL

**🌍 Idioma: [English](README.md) | Español**

El **miembro VCL** de la familia de aplicaciones de DAF: el mismo arranque que las variantes CLI y
Web, hasta el punto en que la plataforma se queda con el bucle.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Todo, de una vez

```pascal
uses Daf.Extensions.DependencyInjection, Daf.VCL.Application;

var B := TVclApplication.CreateBuilder;
B.Services.AddSingleton<IMyService, TMyService>;
var App := B.Build;
App.Run(TMainForm);          // crea el formulario, y a partir de ahí manda Vcl.Forms.Application
```

`Services`, `Configuration`, `Environment` y `Logging` son los mismos miembros, escritos igual, que
en las variantes CLI y Web. Eso es lo que las hace una familia y no tres cosas parecidas.

---

## Quién es dueño del bucle

Es la línea que parte en dos las cuatro variantes, y conviene saberla antes de escribir el `.dpr`:

| Variante | Quién es dueño del proceso |
|---|---|
| CLI, Web | **DAF.** `Run` arranca, trabaja y para; el `.dpr` acaba cuando `Run` acaba |
| **VCL, FMX** | **La plataforma.** `Vcl.Forms.Application` ya existe y ya tiene el message loop; la variante crea el formulario principal y delega |

Son dos contratos de ciclo de vida, no cuatro sobreescrituras de un mismo `Execute`.

`StartUpTo` es el mismo arranque **sin** entrar en el bucle: arranca el host, inicializa la
plataforma y crea el formulario principal, y vuelve. `Run` es `StartUpTo` más el bucle, así que no
vuelve hasta que el bucle termina, y al salir para el host.

Parar —`Stop`, o `Run` al salir— libera los formularios que posee `Vcl.Forms.Application` antes de
parar el host, así que un formulario que usa servicios al destruirse los encuentra vivos.

---

## Una sola instancia

```pascal
var B := TVclApplication.CreateBuilder;
B.SingleInstance('MiEmpresa.MiAplicacion');
var App := B.Build;
App.Run(TMainForm);
```

`SingleInstance(Id)` declara que solo puede correr una instancia con ese identificador; sin
declararla no hay cerrojo. `Build` toma el cerrojo antes de cargar la configuración y de construir el
contenedor. Si otra instancia lo tiene, `Build` devuelve una aplicación resuelta: `Run` y `StartUpTo`
vuelven sin crear el formulario ni entrar en el bucle, `Services` y `MainForm` valen `nil`, `DafApp`
no la apunta, y el código de salida del proceso pasa a ser `TAppExit.AlreadyRunning`. La aplicación
suelta el cerrojo al liberarse, y una construcción que falla también lo suelta.

Es solo el cerrojo: traer al frente la instancia que corre, o pasarle los argumentos de la segunda,
queda en manos de la aplicación.

---

## El global `DafApp`

```pascal
// En código de formulario, que la plataforma construye por su cuenta y al que nadie le puede
// inyectar nada:
var Service := DafApp.Services.GetRequiredService<IMyService>;
```

`DafApp` devuelve la aplicación en curso, y existe solo en las variantes VCL y FMX — para justo el
código que el contenedor nunca llega a construir. En CLI y en Web no hay tal global, y .NET tampoco
lo tiene.

Su nombre **no** es `Application` porque en el mismo `.dpr` conviven éste y
`Vcl.Forms.Application`: ahí un `Application` sin cualificar es el de la VCL, y el de DAF hay que
escribirlo `DafApp`.

**La vía preferente sigue siendo la inyección.** `DafApp` es la salida para el código que el
contenedor nunca ve, no un rodeo para evitarlo.

---

## Sample

- [`TicTacToeApp`](samples/TicTacToeApp/) — la variante entera en un `.dpr`: el contenedor cablea
  el view model, `Vcl.Forms.Application` se queda con el bucle, y el formulario llega al proveedor
  por `DafApp`. Su carpeta `Specs/` ejercita el dominio con MiniSpec.

---

## Dónde diverge de .NET

El miembro de escritorio de .NET es `MauiAppBuilder`, un tipo aparte y no un flag sobre el host
genérico. La divergencia aquí es `Run(TFormClass)`: **una interfaz de Delphi no admite métodos
genéricos**, así que no hay `Run<TForm>` — el mismo límite que deja `ConfigureContainer` sin portar.
