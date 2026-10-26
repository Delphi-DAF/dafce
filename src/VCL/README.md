# Daf.VCL

**🌍 Language: English | [Español](README.es.md)**

The **VCL member** of DAF's application family: the same startup as the CLI and Web variants, up to
the point where the platform takes over the loop.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## The whole thing

```pascal
uses Daf.Extensions.DependencyInjection, Daf.VCL.Application;

var B := TVclApplication.CreateBuilder;
B.Services.AddSingleton<IMyService, TMyService>;
var App := B.Build;
App.Run(TMainForm);          // creates the form, then Vcl.Forms.Application takes over
```

`Services`, `Configuration`, `Environment` and `Logging` are the same members, written the same way,
as in the CLI and Web variants. That is what makes them a family rather than three similar things.

---

## Who owns the loop

This is the line that splits the four variants in two, and it is worth knowing before writing the
`.dpr`:

| Variant | Who owns the process |
|---|---|
| CLI, Web | **DAF.** `Run` starts, works and stops; the `.dpr` ends when `Run` ends |
| **VCL, FMX** | **The platform.** `Vcl.Forms.Application` already exists and already has the message loop; the variant creates the main form and delegates |

They are two lifecycle contracts, not four overrides of one `Execute`.

`StartUpTo` is the same startup **without** entering the loop: it starts the host, initializes the
platform and creates the main form, and returns. `Run` is `StartUpTo` plus the loop, so it does not
return until the loop ends, and it stops the host on the way out.

Stopping — `Stop`, or `Run` on its way out — frees the forms `Vcl.Forms.Application` owns before it
stops the host, so a form that uses services while it is destroyed finds them alive.

---

## A single instance

```pascal
var B := TVclApplication.CreateBuilder;
B.SingleInstance('MyCompany.MyApp');
var App := B.Build;
App.Run(TMainForm);
```

`SingleInstance(Id)` declares that only one instance with that id runs at a time; without it there is
no lock. `Build` takes the lock before loading the configuration and building the container. When
another instance holds it, `Build` returns a settled application: `Run` and `StartUpTo` return
without creating the form or entering the loop, `Services` and `MainForm` are `nil`, `DafApp` does
not point at it, and the process exit code becomes `TAppExit.AlreadyRunning`. The application
releases the lock when it is freed, and a build that fails releases it too.

It is only the lock: bringing the running instance to the front, or handing it the second one's
arguments, is up to the application.

---

## The `DafApp` global

```pascal
// In form code, which the platform builds on its own and nobody can inject into:
var Service := DafApp.Services.GetRequiredService<IMyService>;
```

`DafApp` returns the running application, and it exists in the VCL and FMX variants only — for
exactly the code the container never gets to build. In CLI and Web there is no such global, and .NET
has none either.

Its name is **not** `Application` because in the same `.dpr` this one and `Vcl.Forms.Application`
coexist: an unqualified `Application` there is the VCL's, and DAF's has to be written `DafApp`.

**The preferred route is still injection.** `DafApp` is the way out for code the container never
sees, not a way around the container.

---

## Sample

- [`TicTacToeApp`](samples/TicTacToeApp/) — the whole variant in one `.dpr`: the container wires
  the view model, `Vcl.Forms.Application` gets the loop, and the form reaches the provider through
  `DafApp`. Its `Specs/` folder runs the domain under MiniSpec.

---

## Where it diverges from .NET

.NET's desktop member is `MauiAppBuilder`, a separate type and not a flag on the generic host. The
divergence here is `Run(TFormClass)`: **a Delphi interface admits no generic methods**, so there is no
`Run<TForm>` — the same limit that keeps `ConfigureContainer` from being ported.
