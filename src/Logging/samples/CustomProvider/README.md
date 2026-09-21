# CustomProvider — escribir un proveedor de log propio

Dos proveedores escritos desde fuera del framework, compuestos en el mismo pipeline, y todo lo que
`Daf.Logging` pone alrededor: plantillas con huecos, ámbitos, la puerta `IsEnabled`, `ILogger<T>` y
los dos finales de una composición sin destinos.

Es el sample de lo que el README de `Logging.Abstractions` promete a «quien implemente proveedores
propios», y no había ninguno.

## Qué enseña

- **Un proveedor propio son dos tipos y muy poco código.** `TPrefixLogger` hereda de `TLogger`, así
  que recibe hecha la superficie entera de `ILogger` —las cuatro sobrecargas por nivel, los
  ámbitos— y solo pone dónde escribe y hasta dónde. El proveedor es un `ILoggerProvider` con un
  método.
- **`IsEnabled` es una puerta con dos guardas, y la del pipeline la abre el más permisivo.**
  `TLogger.Log` la consulta antes de analizar la plantilla, y el llamante puede consultarla antes
  de construir un argumento caro. Pero el logger compuesto contesta que sí en cuanto **alguno** de
  sus destinos lo dice, y un destino que no sobrescribe `IsEnabled` contesta `True` por defecto —
  perder una línea por no saber es peor que componer una de más. Con un solo destino así, la
  puerta no cierra nunca. Aquí los dos la contestan, y el argumento caro se construye **una vez de
  dos intentos**.
- **`Entry.Message` ya trae el ámbito pegado detrás.** Un proveedor que además escriba
  `Entry.Scope` lo imprime dos veces. Los ámbitos anidados salen del más interno al más externo.
- **`ILogger<T>` llega con la categoría puesta.** `TPayrollService` pide `ILogger<TPayrollService>`
  por constructor y la categoría que sale es el nombre cualificado de la clase. Nadie escribe ese
  nombre en ningún sitio: `AddLogging` registra **un** `ILogger<_>` abierto para todos los
  servicios.
- **Componer sin un solo proveedor falla, y el silencio deliberado tiene nombre.**
  `ELoggingCompositionError` distingue el descuido de la decisión; `AddNullProvider` es cómo se
  dice que el silencio se quería.

## Ejecutar

```
BDS make --project:src/Logging/samples/CustomProvider/CustomProvider.dproj
out\Win32\samples\bin\CustomProvider.exe
```

Salida:

```
== Una plantilla con huecos, y sus pares clave-valor
  con [INFO ] Acme.Facturacion                           Factura 1042 por 350,5 EUR
    Number   = 1042
    Amount   = 350,5

== Un ambito viaja con todo lo que se escriba dentro
  con [WARN ] Acme.Facturacion                           el cliente no tiene direccion | peticion=7
  con [ERROR] Acme.Facturacion                           no se pudo enviar | reintento=2 | peticion=7

== IsEnabled es una puerta, no un adorno
  con [ERROR] Acme.Facturacion                           estado: volcado-de-estado
    el argumento caro se construyo 1 vez/veces de 2 intentos

== Un servicio recibe un registrador que ya lleva su categoria
  con [INFO ] CustomProvider.Pipeline.TPayrollService    Nomina de 12 empleado(s)

== Una entrada, dos destinos
    el destino contador lleva 5 entrada(s)

== Componer sin un solo proveedor es un error, no un silencio
    AddLogging was given no providers: the pipeline would accept every entry and discard it. Register a provider, or if the silence is deliberate say so with Builder.AddNullProvider.

== Y el silencio deliberado se dice en voz alta
    compuesto con AddNullProvider: la linea se escribe y se descarta
```

Para un proveedor con destinos y reglas ya hechos —consola, fichero, UDP, filtrado por categoría—
está `NNLog`, y su sample `NNLog/samples/NNLogTargets`.
