# Reporters

**🌍 Idioma: [English](reporters.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Sintaxis: `-r <nombre>:<opcion1>=<valor>,<opcion2>=<valor>,...`

| Reporter | Opciones | Ejemplo |
|----------|----------|---------|
| `console` | *(ninguna)* | `-r console` |
| `json` | `output=<file>` | `-r json:output=report.json` |
| `junit` | `output=<file>` | `-r junit:output=results.xml` |
| `gherkin` | `output=<dir>` | `-r gherkin:output=features/` |
| `live` | `port=<num>`, `wait=<ms>` | `-r live:port=8080,wait=5000` |

## Múltiples Reporters

Puedes usar varios reporters en la misma ejecución repitiendo la opción `-r`:

```bash
# Consola + JUnit para CI + JSON para análisis
MiApp.exe -r console -r junit:output=results.xml -r json:output=report.json
```

Todos los reporters reciben los mismos eventos y generan su salida simultáneamente.

## Filtrado y supresión de features

Cuando se usa `-f`, los reporters solo incluyen features que tienen al menos un escenario ejecutado. Las features completamente filtradas se omiten silenciosamente — de la consola, del XML JUnit, del JSON y de los ficheros Gherkin.

## JUnit Reporter

Genera XML en formato JUnit para integración CI/CD. Compatible con GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

Cada escenario que se ejecutó, y cada fila de un Scenario Outline, es un caso. Uno fallado o
Undefined lleva un `<failure>`, y uno Pending un `<skipped>`, cuyo mensaje nombra el step que lo dejó
así. Un escenario que no se ejecutó, como uno que el filtro dejó fuera, no se lista.

Cada hook que lanzó, de la suite o de una feature, es un caso con un `<failure>` que lleva la
descripción del hook y su mensaje. Los de una feature van en su bloque, y los de la suite en un bloque
con el nombre de la suite. Los atributos cuentan los casos que se listan: `failures` recoge los
escenarios fallados y Undefined y los hooks, y `skipped` los Pending. `--allow-undefined` y
`--strict-pending` cambian el código de salida, no el informe. Cucumber, desde la 6.11, da un escenario
Pending como fallo; MiniSpec lo mantiene como omitido, igual que su código de salida.

## JSON Reporter

Cada escenario, cada fila de un Scenario Outline y cada Background lleva un `status`: `pass`, `fail`,
`pending`, `undefined`, o `skip` si no se ejecutó. Cada step lleva un `status` con los nombres del
formateador JSON de Cucumber, `passed`, `failed`, `skipped`, `pending` y `undefined`, y `success`,
verdadero solo para un step que pasó. La raíz lleva los totales: `passCount`, `failCount`,
`skipCount`, `pendingCount` y `undefinedCount`.

## Live Reporter

Por defecto espera 3 segundos para conexión del navegador. Usa `wait=0` para deshabilitar.

## Archivo de Configuración

MiniSpec crea `MiniSpec.ini` en el directorio del ejecutable si no existe:

```ini
[minispec]
reporters=console,live
filter=@unit
pause=true

[reporter.live]
port=8080
wait=3000

[reporter.junit]
output=results.xml
```

La clave `reporters` acepta múltiples nombres de reporter separados por comas. Cada reporter puede tener su propia sección `[reporter.<nombre>]` para opciones.

Los reporters declarados en secciones `[reporter.X]` se registran automáticamente aunque no estén listados en la clave `reporters`. Por ejemplo, lo siguiente equivale a `reporters=console,live`:

```ini
[minispec]
reporters=console

[reporter.live]
port=9000
```

> **Compatibilidad**: la clave antigua `reporter=` (singular) sigue siendo leída.

Las opciones de línea de comandos tienen prioridad sobre el archivo.

---

[← Inyección de Dependencias](injection.es.md) | [Siguiente: Línea de Comandos →](cli.es.md)
