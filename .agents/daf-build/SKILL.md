---
name: daf-build
description: Comandos de compilación, test y ejecución del monorepo DAFce. Incluye cómo conectar a la VM Windows, compilar el framework, samples y tests, y ejecutar los binarios resultantes. Usar cuando se va a compilar, hacer build, lanzar tests o ejecutar samples.
license: MIT
metadata:
  author: Jorge L. Cangas
  email: delphi-daf@proton.me
  repo: Delphi-DAF/dafce
  version: "1.0"
---

# DAFce — Build & Test

## Conectar al entorno

> **Solo necesario desde máquinas sin Delphi instalado** (p.ej. macOS).
> Si ya estás en Windows con Delphi disponible, omite este paso.

```bash
# Conexión a la VM Windows donde está Delphi
# $RS_CLI_MACHINE debe apuntar al host/alias de la máquina Windows con con Rad Studio instalado y configurado para acceder al código (p.ej. mediante carpeta compartida o repositorio clonado).
ssh $RS_CLI_MACHINE

# Navegar al proyecto
# $RS_CLI_PROJECT_PATH es la ruta del proyecto vista desde $RS_CLI_MACHINE
pushd $RS_CLI_PROJECT_PATH
```

## Framework (código fuente)

```bash
# Build incremental / completo / limpieza de un módulo
BDS make  --project:src/[Module]/Daf.[Module].dproj
BDS build --project:src/[Module]/Daf.[Module].dproj
BDS clean --project:src/[Module]/Daf.[Module].dproj

# Build de todo el framework
BDS make --project:src/DAFGroup.groupproj
```

## Samples

```bash
# Build TODOS los samples (transversal)
BDS make --project:src/DAFSamples.groupproj

# Build samples de un módulo
BDS make --project:src/[Module]/[Module].Samples.groupproj

# Build un sample individual
BDS make --project:src/[Module]/samples/[SampleName]/[SampleName].dproj
```

## Tests

```bash
# Build TODOS los tests (transversal)
BDS make --project:src/DAFTestGroup.groupproj

# Build tests de un módulo
BDS make --project:src/[Module]/[Module].Tests.groupproj

# Build test individual
BDS make --project:src/[Module]/test/[Module]Specs.dproj
```

## Directorios de salida

```
out/
├── bin/            # DCUs y unidades compiladas del framework
├── lib/            # Librerías del framework
├── samples/
│   ├── bin/        # Ejecutables de samples
│   └── lib/        # DCUs de samples
└── test/
    ├── bin/        # Ejecutables de tests
    └── lib/        # DCUs de tests
```

## Ejecutar tests y samples

```bash
# Ejecutar un sample o test
out\samples\bin\[SampleName].exe
out\test\bin\[Module]Specs.exe

# Filtrar por tag
out\test\bin\[Module]Specs.exe -f "@tag"

# Reporter JUnit (CI/CD)
out\test\bin\[Module]Specs.exe -r junit:output=results.xml
```

Para todas las opciones del runner (reporters, filtros, stack trace, live dashboard) ver [references/minispec-runner.md](references/minispec-runner.md).
