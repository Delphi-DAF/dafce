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

### Variables de entorno

Las variables necesarias están en **`.env`** en la raíz del proyecto.
**Cárgalas siempre antes de ejecutar cualquier comando SSH:**

```bash
source .env
# Define:
#   RS_CLI_MACHINE      — host/alias SSH de la VM Windows con Rad Studio
#   RS_CLI_PROJECT_PATH — ruta del proyecto en Windows (ej. Z:\ProjectsWK\jcangas\DAFce)
```

> **Unidad de red Z:** — `$RS_CLI_PROJECT_PATH` apunta **físicamente al disco del Mac**.
> Los cambios en la copia de trabajo Mac son **inmediatamente visibles** desde Windows,
> sin necesidad de `git push/pull` ni ningún paso de sincronización.

### Sintaxis SSH correcta

Los comandos se lanzan con `cmd /c` y las barras invertidas de la ruta deben escaparse:

```bash
source .env
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && <comando>\""
```

Ejemplo concreto:

```bash
source .env
ssh $RS_CLI_MACHINE "cmd /c \"pushd Z:\\ProjectsWK\\jcangas\\DAFce && BDS make --project:src/SQLCute.Abstractions/test/SQLCuteSpecs.dproj\""
```

## Framework (código fuente)

```bash
source .env

# Build incremental / completo / limpieza de un módulo
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make  --project:src/[Module]/Daf.[Module].dproj\""
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS build --project:src/[Module]/Daf.[Module].dproj\""
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS clean --project:src/[Module]/Daf.[Module].dproj\""

# Build de todo el framework
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/DAFGroup.groupproj\""
```

## Samples

```bash
source .env

# Build TODOS los samples (transversal)
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/DAFSamples.groupproj\""

# Build samples de un módulo
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/[Module]/[Module].Samples.groupproj\""

# Build un sample individual
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/[Module]/samples/[SampleName]/[SampleName].dproj\""
```

## Tests

```bash
source .env

# Build TODOS los tests (transversal)
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/DAFTestGroup.groupproj\""

# Build tests de un módulo
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/[Module]/[Module].Tests.groupproj\""

# Build test individual
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && BDS make --project:src/[Module]/test/[Module]Specs.dproj\""
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
source .env

# Ejecutar un sample o test
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && out\\samples\\bin\\[SampleName].exe\""
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && out\\test\\bin\\[Module]Specs.exe\""

# Filtrar por tag
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && out\\test\\bin\\[Module]Specs.exe -f \"@tag\"\""

# Reporter JUnit (CI/CD)
ssh $RS_CLI_MACHINE "cmd /c \"pushd $RS_CLI_PROJECT_PATH && out\\test\\bin\\[Module]Specs.exe -r junit:output=results.xml\""
```

Para todas las opciones del runner (reporters, filtros, stack trace, live dashboard) ver [references/minispec-runner.md](references/minispec-runner.md).
