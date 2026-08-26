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
B.Services.AddModule<TTicTacToeModule>;
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

Son dos contratos de ciclo de vida, no cuatro sobreescrituras de un mismo `Execute`. Tratarlas igual
esconde esa diferencia justo donde más cuesta descubrirla.

`StartUpTo` es el mismo arranque **sin** entrar en el bucle. Existe para que la variante se pueda
ejercitar desde un spec: llamar a `Run` en una suite entraría en el message loop y se quedaría ahí, y
una spec colgada no es una spec roja.

---

## El global, y por qué esta variante tiene uno

```pascal
// En código de formulario, que la plataforma construye por su cuenta y al que nadie le puede
// inyectar nada:
var Service := DafApp.Services.GetRequiredService<IMyService>;
```

`DafApp` existe en VCL y en FMX **solo porque la plataforma ya tiene un global así y el usuario lo
espera**. Negárselo obligaría a inventar un mecanismo peor para justo el código al que no se le puede
inyectar. En CLI y en Web no hay tal global, .NET tampoco lo tiene, y añadirlo sería un service
locator sin nada que lo justifique.

Su nombre **no** es `Application` a propósito: en el mismo `.dpr` conviven éste y
`Vcl.Forms.Application`, y el choque no es hipotético — un sample del propio módulo de aplicación
escribía `Application.Run` refiriéndose al de la VCL.

**La vía preferente sigue siendo la inyección.** `DafApp` es la salida para el código que el
contenedor nunca ve, no un rodeo para evitarlo.

---

## Dónde diverge de .NET

El miembro de escritorio de .NET es `MauiAppBuilder`, y el precedente se sostiene: es un tipo aparte y
no un flag sobre el host genérico, por esta misma razón. La divergencia aquí es `Run(TFormClass)` en
vez de `Run<TForm>` — **una interfaz de Delphi no admite métodos genéricos**, el mismo límite que deja
`ConfigureContainer` sin portar.
