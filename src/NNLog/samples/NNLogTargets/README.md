# NNLogTargets — reglas, destinos y un destino escrito por ti

Un proveedor de NNLog compuesto **con código** en vez de leído de un fichero, con un destino propio
al lado del de consola, y los cuatro comportamientos que más se malentienden: el `name` de una
regla, `final`, los tokens del layout y lo que pasa cuando un destino no puede escribir.

## Qué enseña

- **Un destino propio son una clase y un nombre de unit, y el nombre no es estilo.** El cargador
  resuelve un `targetType` por RTTI a `Daf.NNLog.Targets.<Tipo>.T<Tipo>Target`, distinguiendo
  mayúsculas. `TAuditTarget` vive en `Daf.NNLog.Targets.Audit.pas` porque su `targetType` es
  `Audit`.
- ⚠️ **Y la unit del destino tiene que estar en el `uses` del programa.** El RTTI solo ve lo que el
  enlazador metió, así que un destino que nadie nombra —incluido `Console`— no se encuentra y la
  carga aborta con `Unknown target type`. Con `{$STRONGLINKTYPES ON}` tampoco basta: hay que
  nombrarlo.
- **Un destino implementa `DoWrite`, no `Write`.** `TTarget.Write` toma un candado propio de esa
  instancia y luego llama a `DoWrite`, así que dos hilos que escriban por el mismo destino no se
  solapan sin que el destino haga nada.
- **Y si `DoWrite` lanza, quien registraba no se entera.** El fallo sale por el canal de arranque,
  nombrando al destino —por su `Name`, o por su clase si no tiene—, y la siguiente línea se escribe con normalidad. El sample lo enseña
  entero: el programa sigue vivo y el informe aparece.
- **El canal de arranque trae puestos dos destinos, la salida de depuración y la de error.** Es donde
  informa el logging cuando lo que falla es el logging, y quien despliega le añade los suyos.
  `IBootstrapSink` son dos métodos: `TStdOutBootstrapSink`, en el `.dpr`, es uno entero.
- **El `name` de una regla es una expresión regular anclada a la categoría completa**, no un glob:
  `Acme\..*` casa con `Acme.Orders` y no con `Otros.Cosas`. Es una divergencia deliberada con NLog.
- **`final` corta el recorrido.** `Acme.Audit` casa con las dos reglas; la de auditoría va primera
  y es final, así que su línea no llega a consola. El orden de las reglas es el orden del array.
- **El ámbito se pide con su propio token.** `${message}` es el mensaje y nada más; `${scope}`
  escribe los ámbitos abiertos, cada uno precedido de ` | `. El destino de auditoría lo pide detrás
  del mensaje, `${message}${scope}`.
- **Un token sin renderer se queda sin sustituir.** `${nada}` sale tal cual, que es lo que hace
  visible un `${mesage}` mal escrito en vez de dejar un hueco en blanco.
- **Escribir los ajustes o leerlos de una sección da lo mismo.** La última sección compone el
  mismo destino de consola desde un `IConfiguration` y escribe la misma línea.

Para un proveedor escrito desde cero, sin destinos ni reglas hechos, está
`Logging/samples/CustomProvider`.

## Ejecutar

```
BDS make --project:src/NNLog/samples/NNLogTargets/NNLogTargets.dproj
out\Win32\samples\bin\NNLogTargets.exe
```

Salida, con la fecha, el pid y el hilo del momento:

```
== El nombre de una regla es una expresion regular sobre la categoria entera
  [WARN] Acme.Orders | stock por debajo del minimo

== Una regla final corta el recorrido
  [AUD] Acme.Audit pedido=4711 | pago aceptado 4711 ${nada}

== Los tokens del layout, y uno que no existe
  [AUD] Acme.Audit pedido=99 | cierre de lote 99 | lote=12 ${nada}

== Un destino que no puede escribir ya no tumba a quien registraba
2026-09-21 15:37:39.137 #8000:7716 [ERROR] TAuditTarget | el destino "audit" no pudo escribir Exception: el almacen de auditoria no responde
  >> 2026-09-21 15:37:39.137 #8000:7716 [ERROR] TAuditTarget | el destino "audit" no pudo escribir Exception: el almacen de auditoria no responde
  el programa sigue vivo, y la siguiente linea si se escribe:
  [AUD] Acme.Audit pedido=4713 | pago aceptado 4713 ${nada}

== Los mismos ajustes, leidos de una seccion
  [WARN] Acme.Orders | stock por debajo del minimo
```

El layout de los destinos va **sin `${timestamp}`** para que la salida sea la misma en cada pasada;
el del canal de arranque es fijo y sí lo lleva. El informe del destino que falla sale dos veces: por
la salida de error, que el canal trae puesta, y por el destino del sample, con `>>` delante.
