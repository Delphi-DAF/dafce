# Daf.FMX — Lo que se apoya en FireMonkey

**🌍 Idioma: [English](README.md) | Español**

Las implementaciones de DAF que necesitan FireMonkey, en un paquete propio para que ese stack le llegue solo a quien lo pide. Hoy hay una: `TPlatformLogBootstrapSink`, el destino del canal de arranque que escribe por el servicio de log de la plataforma.

**Este paquete existe para que fuera de Windows el informe de arranque no se pierda en silencio.** **No** es soporte de plataforma. Se parecen al leerlos y no son lo mismo: que un destino pregunte a la plataforma si puede escribir arregla que **se sepa** cuándo no puede, no hace que DAF funcione fuera de Windows. Los targets declarados del framework son **Win32 y Win64**, y ningún proyecto suyo declara otro.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Registrar el destino

El canal de arranque recibe sus destinos antes de construir el host, y este no va registrado de serie — referenciar el paquete es la forma de pedirlo:

```pascal
uses DAF.Extensions.Logging, Daf.FMX.BootstrapSink;

begin
  BootstrapLog.AddSink(TPlatformLogBootstrapSink.Create);
  // ... construir el host; desde aquí un fallo ya tiene dónde informarse
end.
```

La disponibilidad la responde la plataforma, no un `{$IFDEF}` que alguien mantiene a mano: `IsAvailable` le pide `IFMXLoggingService` a `TPlatformServices` y dice lo que le contestaron. Donde no hay ese servicio, el destino **se declara no disponible** en vez de aceptar el informe y perderlo.

---

## Por qué un paquete propio

`Daf.Logging.Abstractions` es el paquete de contratos: lo usa todo el mundo y su restricción es no depender de nada, así que FireMonkey estaría ahí en el peor sitio posible. `Daf.Logging` lo requiere quien quiere logging, no quien quiere FireMonkey, así que ponerlo ahí arrastraría FMX a toda aplicación VCL que use el pipeline.

Una frontera se justifica cuando una capacidad opcional arrastra una dependencia que los demás no necesitan. Esta la arrastra, y la regla se comprueba en vez de confiarse: `eng/check-packages.py` falla la construcción si cualquier paquete que no sea este enlaza `FMX.*`, sea por `requires` o por importación implícita.

---

## Relacionado

- [Logging.Abstractions](../Logging.Abstractions/README.es.md) — el contrato `IBootstrapSink` y el canal de arranque
- [Composición del arranque](../DependencyInjection/docs/STARTUP-COMPOSITION.es.md) — la tabla de destinos y qué informa cada fase
