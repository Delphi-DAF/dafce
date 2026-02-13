# Línea de Comandos

**🌍 Idioma: [English](cli.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

| Opción | Descripción |
|--------|-------------|
| `-h, --help` | Muestra ayuda |
| `-f, --filter <expr>` | Filtra escenarios (ver [Tags y Filtrado](tags-filtering.es.md)) |
| `-t, --tags` | Lista todos los tags con conteos |
| `-q, --query <expr>` | Muestra escenarios que coinciden (sin ejecutar) |
| `-r, --reporter <spec>` | Reporter con opciones (ver [Reporters](reporters.es.md)) |
| `--pause` | Espera tecla al finalizar |
| `--dry-run` | Lista escenarios sin ejecutarlos |
| `--stacktrace` | Muestra stack trace completo en errores |

> 💡 `--stacktrace` requiere una librería de stack traces (JclDebug, MadExcept, EurekaLog).

## Ejemplos

```bash
# Ayuda
MiApp.exe -h

# Ejecutar solo tests unitarios
MiApp.exe -f "@unit"

# Ver qué escenarios coinciden sin ejecutar
MiApp.exe -q "@integration"

# Listar todos los tags
MiApp.exe -t

# Ejecutar con JUnit output para CI
MiApp.exe -r junit:output=results.xml

# Dashboard en tiempo real
MiApp.exe -r live:port=8080

# Múltiples reporters simultáneamente
MiApp.exe -r console -r junit:output=results.xml -r json:output=report.json
```

## Archivo de Configuración

Si no existe un archivo `MiniSpec.ini` en el directorio del ejecutable, MiniSpec crea uno con opciones por defecto en la primera ejecución. En ejecuciones posteriores, las opciones se cargan de este archivo.

Las opciones de línea de comandos **siempre tienen prioridad** sobre el archivo. Ver [Reporters](reporters.es.md) para la sintaxis completa del INI.

```ini
[minispec]
reporters=console,live
filter=@unit
pause=true

[reporter.live]
port=8080
```

---

[← Reporters](reporters.es.md) | [Volver a la Guía →](../GUIDE.es.md)
