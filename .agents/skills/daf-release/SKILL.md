---
name: daf-release
description: Proceso de release del repositorio DAFce. Usar cuando se va a preparar o publicar una nueva versión.
license: MIT
metadata:
  author: Jorge L. Cangas
  email: delphi-daf@proton.me
  repo: Delphi-DAF/dafce
  version: "1.0"
---

# DAFce — Release

## Comandos

```powershell
# Preparar release: bump de versión, actualiza CHANGELOG, commit
scripts/release.ps1 prepare [major|minor|patch]

# Publicar: merge a main, tag, push, crea GitHub release
scripts/release.ps1 publish

# Ambos admiten -DryRun para simular sin aplicar cambios
scripts/release.ps1 prepare patch -DryRun
scripts/release.ps1 publish -DryRun
```

## Flujo completo

1. Asegurarse de que todos los tests pasan
2. `release.ps1 prepare [major|minor|patch]` — revisa el CHANGELOG generado y el bump de versión
3. `release.ps1 publish` — fusiona a `main`, crea el tag y publica el release en GitHub
