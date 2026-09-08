# Ejecuciones anidadas

**🌍 Idioma: [English](nested-runs.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Un `TSpecRun` es una ejecución autocontenida: posee su suite, su contexto, su inyector, sus opciones
y su runner con sus listeners. Se puede lanzar **dentro** de un escenario en marcha sin alterar los
recuentos de la ejecución que lo contiene y sin escribir una línea en su informe.

Eso es lo que hace que las reglas del runner se puedan comprobar con specs en vez de a mano.

## Montar una y ejecutarla

```pascal
uses Daf.MiniSpec, Daf.MiniSpec.Types, Daf.MiniSpec.Runner;

type
  TToyWorld = class
  public
    Touched: Boolean;
  end;

var Run := TSpecRun.Create('Nested');
try
  Feature('A toy feature that fails', Run.Suite)
    .UseWorld<TToyWorld>
    .Scenario('It fails on purpose')
      .&Then('one is not two', procedure(W: TToyWorld)
        begin
          Expect(1).ToEqual(2);
        end);

  Run.Execute;

  Expect(Run.FailCount).ToEqual(1);   // la anidada contó su propio fallo
  Expect(Run.ExitCode).ToEqual(1);    // y dice qué código le corresponde a sus recuentos
  Expect(ExitCode).ToEqual(0);        // el del proceso queda intacto
finally
  Run.Free;
end;
```

`TSpecRun` es una clase normal, así que la libera quien la creó — habitualmente el World del
escenario que la ejecuta.

`Feature(Descripción, Suite)` es la sobrecarga que declara la feature **en la suite que se le
indica** en vez de en la del proceso. Es la única vía por la que un árbol de features llega a una
ejecución anidada: el `Feature` de un argumento declara siempre en la suite del proceso.

## Lo que se queda fuera de una ejecución anidada

El reparto con `MiniSpec` sigue una sola regla: lo que solo tiene sentido una vez por proceso se
queda en la fachada — los argumentos de línea de comandos, el `.ini`, el banner y el código de
salida del proceso.

`Run.ExitCode` es el código que le corresponde a esa ejecución, como función de sus propios
recuentos. Quien la lanzó decide qué hacer con él; solo la ejecución raíz lo lleva al proceso.

## Informes

Una ejecución anidada no imprime nada hasta que se le da un destino:

```pascal
Run.AddListener(Listener);
```

Sin ningún listener no hay salida, y eso es lo que permite anidar una ejecución dentro de un
escenario sin ensuciar su informe. Los destinos disponibles están en [Reporters](reporters.es.md).

## Su propio contexto de suite

```pascal
Run.UseSuiteContext<TMyContext>;
```

Fija cómo se construye el contexto de suite de **esta** ejecución. Una excepción que escape de ese
constructor es el único caso que escapa de `Run.Execute`: lo que revienta dentro de un escenario lo
atrapa el propio runner y lo cuenta como fallo.

## Qué informa una ejecución anidada

| Miembro | Responde |
|---------|----------|
| `Suite` | La suite de esta ejecución — lo que toma `Feature(Descripción, Suite)` |
| `Runner` | Su runner, con sus propios listeners |
| `Options` | Las opciones de esta ejecución, independientes de las del proceso |
| `Injector` | Su inyector |
| `PassCount` `FailCount` `SkipCount` `PendingCount` `UndefinedCount` `TotalCount` | Sus propios recuentos |
| `FeatureCount` | Features declaradas en su suite |
| `ElapsedMs` | Lo que tardó esta ejecución |
| `ExitCode` | El código que le corresponde a sus recuentos |

---

[← Línea de Comandos](cli.es.md) | [Volver a la Guía →](../GUIDE.es.md)
