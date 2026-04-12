---
name: daf-conventions
description: Convenciones de nomenclatura y estructura para specs MiniSpec en DAFce. Usar cuando se crean o modifican ficheros de specs (Feat.pas, Steps.pas), se configura un proyecto de tests, o se trabaja con RTTI y generics en MiniSpec.
license: MIT
metadata:
  author: Jorge L. Cangas
  email: delphi-daf@proton.me
  repo: Delphi-DAF/dafce
  version: "1.0"
---

# DAFce — Convenciones MiniSpec

## Nomenclatura de ficheros

| Tipo | Patrón | Ejemplo |
|------|--------|---------|
| Feature spec | `[Feature].Feat.pas` | `Activator.Feat.pas` |
| Step definitions | `[Feature].Steps.pas` | `Activator.Steps.pas` |
| Test doubles | `Doubles.Feat.pas` | `Doubles.Feat.pas` |
| Spec runner (`.dpr`) | `[Module]Specs.dpr` | `CommonsSpecs.dpr` |
| Proyecto de tests | `[Module]Specs.dproj` | `CommonsSpecs.dproj` |
| Groupproj de tests | `[Module].Tests.groupproj` | `Commons.Tests.groupproj` |

## Estructura de un módulo con tests

```
src/[Module]/
├── [Module].Tests.groupproj
└── test/
    ├── [Module]Specs.dpr
    ├── [Module]Specs.dproj
    ├── [Feature].Feat.pas
    ├── [Feature].Steps.pas   # si hay steps separados
    └── Doubles.Feat.pas       # si hay test doubles
```

## Configuración del `.dpr` del spec runner

### RTTI y tipos descubiertos vía `DiscoverImpl`

Si el módulo usa `DiscoverImpl` o scanning RTTI (e.g. MediatR handlers), añadir al `.dpr`:

```pascal
{$STRONGLINKTYPES ON}
program MediatRSpecs;
...
```

Sin `{$STRONGLINKTYPES ON}` el linker puede eliminar tipos que no se referencian directamente, rompiendo el descubrimiento en runtime.

### Handlers MediatR en specs

Los tipos que se descubren vía RTTI deben declararse en la sección `interface` de su unidad (no solo en `implementation`), para que el linker los incluya.
