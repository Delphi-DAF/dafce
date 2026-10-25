# Daf.FMX — Lo que se apoya en FireMonkey

**🌍 Idioma: [English](README.md) | Español**

Las implementaciones de DAF que necesitan FireMonkey, en un paquete propio para que ese stack le llegue solo a quien lo pide. Hay **dos**, y son las dos razones de ser de este paquete:

- `TPlatformLogBootstrapSink`, el destino del canal de arranque que escribe por el servicio de log de la plataforma — para que **fuera de Windows el informe de arranque no se pierda en silencio**.
- `TFmxApplication`, el **miembro FMX de la familia de aplicaciones**. Vive aquí y no en un paquete propio porque el invariante de arriba —ningún otro paquete de DAF enlaza FireMonkey— no admite un segundo que lo haga.

**Ninguna de las dos es soporte de plataforma.** Se parecen al leerlo y no son lo mismo: que un destino pregunte a la plataforma si puede escribir arregla que **se sepa** cuándo no puede, y **que exista una variante de aplicación FMX tampoco hace que DAF corra ahí** — la variante crea el formulario y le entrega el bucle a `FMX.Forms.Application`, que es un contrato de *ciclo de vida*, no una lista de targets. Los targets declarados del framework son **Win32 y Win64**, y ningún proyecto suyo declara otro.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## La variante de aplicación

```pascal
uses Daf.FMX.Application;

var B := TFmxApplication.CreateBuilder;
B.Services.AddSingleton<IMyService, TMyService>;
var App := B.Build;
App.Run(TMainForm);          // crea el formulario, y a partir de ahí manda FMX.Forms.Application
```

Este `Run` **no** es el de la CLI. En CLI y en Web DAF es dueño del proceso: `Run` arranca, trabaja y para, y el `.dpr` acaba cuando él acaba. Aquí el message loop ya existe y es de `FMX.Forms.Application`; la variante crea el formulario principal y delega. Son dos contratos de ciclo de vida, no cuatro variaciones de uno.

Parar —`Stop`, o `Run` al salir— libera los formularios que posee `FMX.Forms.Application` antes de
parar el host, así que un formulario que usa servicios al destruirse los encuentra vivos.

### El global `DafApp`

`DafApp` devuelve la aplicación en curso. Existe solo en las variantes VCL y FMX — para el código de formulario que la plataforma construye por su cuenta, al que nadie le puede inyectar nada. En CLI y en Web no hay tal global, y .NET tampoco lo tiene.

Su nombre **no** es `Application` a propósito: en el mismo `.dpr` conviven éste y el de la plataforma. **La vía preferente sigue siendo la inyección** — `DafApp` es la salida para el código que el contenedor nunca ve, no un rodeo para evitarlo.

### Una sola instancia

`B.SingleInstance(Id)` declara que solo puede correr una instancia con ese identificador, como en la
variante VCL. Una segunda instancia no construye el contenedor ni crea el formulario, `Run` vuelve
sin entrar en el bucle, y el código de salida del proceso pasa a ser `TAppExit.AlreadyRunning`.

---

## Registrar el destino

El canal de arranque recibe sus destinos antes de construir el host, y este no va registrado de serie — referenciar el paquete es la forma de pedirlo:

```pascal
uses Daf.Extensions.Logging, Daf.FMX.BootstrapSink;

begin
  BootstrapLog.AddSink(TPlatformLogBootstrapSink.Create);
  // ... construir el host; desde aquí un fallo ya tiene dónde informarse
end.
```

La disponibilidad la responde la plataforma: `IsAvailable` le pide `IFMXLoggingService` a `TPlatformServices` y dice lo que le contestaron. Donde no hay ese servicio, el destino **se declara no disponible**, y `BootstrapLog` escribe la línea en los destinos que le quedan.

---

## La frontera de FireMonkey

`Daf.FMX` es el único paquete del framework que enlaza FireMonkey, sea por `requires` o por importación implícita. `Daf.Logging.Abstractions` es el paquete de contratos y no depende de nada; `Daf.Logging` lo requiere quien quiere logging. Ninguno de los dos arrastra FMX, así que una aplicación VCL que use el pipeline de logging de DAF no enlaza FireMonkey — referenciar `Daf.FMX` es la forma de pedirlo.

---

## Relacionado

- [Logging.Abstractions](../Logging.Abstractions/README.es.md) — el contrato `IBootstrapSink` y el canal de arranque
- [Composición del arranque](../DependencyInjection/docs/STARTUP-COMPOSITION.es.md) — la tabla de destinos y qué informa cada fase
