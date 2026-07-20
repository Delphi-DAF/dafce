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
