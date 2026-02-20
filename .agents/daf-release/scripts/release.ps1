<#
.SYNOPSIS
    Script para gestionar versiones y releases de DAFce

.DESCRIPTION
    Permite preparar y publicar releases siguiendo Semantic Versioning.
    Separa la preparación (reversible) de la publicación (irreversible).

.PARAMETER Action
    La acción a realizar: prepare, publish o help

.PARAMETER BumpType
    Tipo de incremento: major, minor o patch (solo para prepare)

.PARAMETER DryRun
    Muestra lo que se haría sin ejecutar ninguna acción

.EXAMPLE
    .\release.ps1 prepare minor -DryRun
    .\release.ps1 prepare minor
    .\release.ps1 publish -DryRun
    .\release.ps1 publish
#>

param(
    [Parameter(Position=0)]
    [ValidateSet("prepare", "publish", "help", "")]
    [string]$Action = "",

    [Parameter(Position=1)]
    [ValidateSet("major", "minor", "patch")]
    [string]$BumpType = "patch",

    [switch]$DryRun
)

function Show-Help {
    Write-Host ""
    Write-Host "DAFce Release Script" -ForegroundColor Cyan
    Write-Host "====================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Uso:" -ForegroundColor Yellow
    Write-Host "  .\release.ps1 <accion> [opciones]"
    Write-Host ""
    Write-Host "Acciones:" -ForegroundColor Yellow
    Write-Host "  prepare <tipo>  Prepara el release: bump version, actualiza CHANGELOG, commit"
    Write-Host "                  Tipos: patch (default), minor, major"
    Write-Host "  publish         Merge a main, tag, push y crea release en GitHub"
    Write-Host "  help            Muestra esta ayuda"
    Write-Host ""
    Write-Host "Opciones:" -ForegroundColor Yellow
    Write-Host "  -DryRun         Muestra lo que se haría sin ejecutar nada"
    Write-Host ""
    Write-Host "Ejemplos:" -ForegroundColor Yellow
    Write-Host "  .\release.ps1 prepare minor -DryRun  # Previsualizar"
    Write-Host "  .\release.ps1 prepare minor          # Preparar release"
    Write-Host "  .\release.ps1 publish -DryRun        # Previsualizar publicación"
    Write-Host "  .\release.ps1 publish                # Publicar release"
    Write-Host ""
    Write-Host "Flujo típico:" -ForegroundColor Yellow
    Write-Host "  1. .\release.ps1 prepare minor -DryRun  # Ver qué pasará"
    Write-Host "  2. .\release.ps1 prepare minor          # Preparar"
    Write-Host "  3. Revisar: git diff HEAD~1"
    Write-Host "  4. .\release.ps1 publish                # Publicar"
    Write-Host ""
}

if ($Action -eq "" -or $Action -eq "help") {
    Show-Help
    exit 0
}

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$VersionFile = Join-Path $ScriptDir "src/VERSION.txt"
$ChangelogFile = Join-Path $ScriptDir "CHANGELOG.md"
$EnvFile = Join-Path $ScriptDir ".env"
$RepoOwner = "Delphi-DAF"
$RepoName = "dafce"
$DefaultBranch = "main"

# --- Helpers ---

function Write-DryRun {
    param([string]$Message)
    Write-Host "  [dry-run] $Message" -ForegroundColor DarkYellow
}

function Write-Step {
    param([string]$Message)
    if ($DryRun) {
        Write-DryRun $Message
    } else {
        Write-Host "  $Message" -ForegroundColor Cyan
    }
}

# --- Funciones auxiliares ---

function Get-CurrentVersion {
    if (-not (Test-Path $VersionFile)) {
        throw "No se encontró el archivo VERSION.txt en $VersionFile"
    }
    $version = (Get-Content $VersionFile -Raw).Trim()
    if ($version -notmatch '^\d+\.\d+\.\d+$') {
        throw "Formato de versión inválido: $version. Se espera X.Y.Z"
    }
    return $version
}

function Set-Version {
    param([string]$Version)
    $Version | Set-Content $VersionFile -NoNewline
}

function Get-GitHubToken {
    if (-not (Test-Path $EnvFile)) {
        throw "No se encontró el archivo .env en $EnvFile"
    }

    $envContent = Get-Content $EnvFile
    foreach ($line in $envContent) {
        if ($line -match '^GITHUB_TOKEN=(.+)$') {
            return $Matches[1].Trim()
        }
    }
    throw "No se encontró GITHUB_TOKEN en .env"
}

function Get-CurrentBranch {
    return (git rev-parse --abbrev-ref HEAD).Trim()
}

function Assert-CleanWorkingTree {
    $status = git status --porcelain
    if ($status) {
        throw "El working tree no está limpio. Haz commit o stash de tus cambios primero."
    }
}

function Invoke-BumpVersion {
    param([string]$Type)

    $currentVersion = Get-CurrentVersion
    $parts = $currentVersion.Split('.')
    $major = [int]$parts[0]
    $minor = [int]$parts[1]
    $patch = [int]$parts[2]

    switch ($Type) {
        "major" { $major++; $minor = 0; $patch = 0 }
        "minor" { $minor++; $patch = 0 }
        "patch" { $patch++ }
    }

    return "$major.$minor.$patch"
}

function Update-Changelog {
    param([string]$Version)

    $date = Get-Date -Format "yyyy-MM-dd"
    $content = Get-Content $ChangelogFile -Raw

    # Verificar que hay sección [Unreleased] con contenido
    if ($content -notmatch '## \[Unreleased\]\s*\r?\n\s*\r?\n###') {
        throw "No se encontró contenido en la sección [Unreleased] del CHANGELOG. Nada que liberar."
    }

    # Reemplazar [Unreleased] por la nueva versión, manteniendo [Unreleased] vacío
    $content = $content -replace `
        '## \[Unreleased\]', `
        "## [Unreleased]`n`n## [$Version] - $date"

    # Actualizar el link de comparación de [Unreleased]
    $content = $content -replace `
        '\[Unreleased\]: https://github.com/[^/]+/[^/]+/compare/v[\d.]+\.\.\.HEAD', `
        "[Unreleased]: https://github.com/$RepoOwner/$RepoName/compare/v$Version...HEAD"

    # Añadir link de comparación para la nueva versión
    $previousVersion = Get-CurrentVersion
    $newLink = "[$Version]: https://github.com/$RepoOwner/$RepoName/compare/v$previousVersion...v$Version"

    # Insertar el nuevo link después del link de [Unreleased]
    $content = $content -replace `
        "(\[Unreleased\]: https://github.com/$RepoOwner/$RepoName/compare/v$Version\.\.\.HEAD)", `
        "`$1`n$newLink"

    Set-Content $ChangelogFile -Value $content.TrimEnd() -NoNewline
}

function Preview-ChangelogUpdate {
    param([string]$Version)

    $date = Get-Date -Format "yyyy-MM-dd"
    $previousVersion = Get-CurrentVersion

    Write-Host ""
    Write-Host "  CHANGELOG.md:" -ForegroundColor White
    Write-Host "    ## [Unreleased] -> ## [Unreleased] (vacío) + ## [$Version] - $date" -ForegroundColor DarkYellow
    Write-Host "    Link [Unreleased]: ...compare/v$previousVersion...HEAD -> ...compare/v$Version...HEAD" -ForegroundColor DarkYellow
    Write-Host "    Link nuevo: [$Version]: ...compare/v$previousVersion...v$Version" -ForegroundColor DarkYellow
}

# --- Comandos principales ---

function Invoke-Prepare {
    param([string]$Type)

    $currentBranch = Get-CurrentBranch
    if ($currentBranch -eq $DefaultBranch) {
        throw "No puedes preparar un release desde '$DefaultBranch'. Usa una rama de feature/release."
    }

    Assert-CleanWorkingTree

    $currentVersion = Get-CurrentVersion
    $newVersion = Invoke-BumpVersion -Type $Type

    # Verificar que el CHANGELOG tiene contenido en [Unreleased]
    $content = Get-Content $ChangelogFile -Raw
    if ($content -notmatch '## \[Unreleased\]\s*\r?\n\s*\r?\n###') {
        throw "No se encontró contenido en la sección [Unreleased] del CHANGELOG. Nada que liberar."
    }

    $label = if ($DryRun) { "Previsualización de release" } else { "Preparando release" }

    Write-Host ""
    Write-Host "$label..." -ForegroundColor Cyan
    Write-Host "  Rama:            $currentBranch" -ForegroundColor White
    Write-Host "  Versión actual:  $currentVersion" -ForegroundColor Yellow
    Write-Host "  Nueva versión:   $newVersion" -ForegroundColor Green
    Write-Host ""

    if ($DryRun) {
        Write-Step "Actualizar CHANGELOG.md"
        Preview-ChangelogUpdate -Version $newVersion
        Write-Host ""
        Write-Step "Actualizar VERSION.txt: $currentVersion -> $newVersion"
        Write-Step "git add src/VERSION.txt CHANGELOG.md"
        Write-Step "git commit -m 'Release $newVersion'"
        Write-Host ""
        Write-Host "No se ha modificado nada." -ForegroundColor Green
        Write-Host ""
    } else {
        Write-Host "  Actualizando CHANGELOG.md..." -ForegroundColor Cyan
        Update-Changelog -Version $newVersion

        Write-Host "  Actualizando VERSION.txt..." -ForegroundColor Cyan
        Set-Version $newVersion

        Write-Host "  Creando commit de release..." -ForegroundColor Cyan
        git add $VersionFile $ChangelogFile
        git commit -m "Release $newVersion"

        Write-Host ""
        Write-Host "Release $newVersion preparado." -ForegroundColor Green
        Write-Host ""
        Write-Host "Próximos pasos:" -ForegroundColor Yellow
        Write-Host "  1. Revisar: git diff HEAD~1"
        Write-Host "  2. Si todo está bien: .\release.ps1 publish"
        Write-Host "  3. Si hay que corregir: git reset HEAD~1 --soft"
        Write-Host ""
    }
}

function Invoke-Publish {
    $currentBranch = Get-CurrentBranch
    if ($currentBranch -eq $DefaultBranch) {
        throw "No puedes publicar desde '$DefaultBranch'. Debes estar en la rama del release."
    }

    Assert-CleanWorkingTree

    $version = Get-CurrentVersion
    $tag = "v$version"

    # Verificar que el último commit es el de release
    $lastCommitMsg = (git log -1 --format="%s").Trim()
    if ($lastCommitMsg -ne "Release $version") {
        throw "El último commit no es 'Release $version' (es '$lastCommitMsg'). Ejecuta primero '.\release.ps1 prepare'."
    }

    $label = if ($DryRun) { "Previsualización de publicación" } else { "Publicando release" }

    Write-Host ""
    Write-Host "$label $tag..." -ForegroundColor Cyan
    Write-Host "  Rama:    $currentBranch -> $DefaultBranch" -ForegroundColor White
    Write-Host "  Versión: $version" -ForegroundColor White
    Write-Host ""

    if ($DryRun) {
        Write-Step "git checkout $DefaultBranch"
        Write-Step "git pull origin $DefaultBranch"
        Write-Step "git merge $currentBranch --no-ff -m 'Merge branch ''$currentBranch'' for release $version'"
        Write-Step "git tag -a $tag -m 'Release $version'"
        Write-Step "git push origin $DefaultBranch"
        Write-Step "git push origin $tag"
        Write-Step "Crear release '$tag' en GitHub via API"
        Write-Step "git checkout $currentBranch"
        Write-Host ""
        Write-Host "No se ha modificado nada." -ForegroundColor Green
        Write-Host ""
        return
    }

    $token = Get-GitHubToken

    $headers = @{
        "Authorization" = "Bearer $token"
        "Accept" = "application/vnd.github+json"
        "X-GitHub-Api-Version" = "2022-11-28"
    }

    # Verificar si la release ya existe en GitHub
    $releaseUri = "https://api.github.com/repos/$RepoOwner/$RepoName/releases/tags/$tag"
    try {
        $existingRelease = Invoke-RestMethod -Uri $releaseUri -Headers $headers -ErrorAction Stop
        Write-Host "La release $tag ya existe: $($existingRelease.html_url)" -ForegroundColor Yellow
        return
    }
    catch {
        # Release no existe, continuamos
    }

    # 1. Merge a main
    Write-Host "  Mergeando $currentBranch -> $DefaultBranch..." -ForegroundColor Cyan
    git checkout $DefaultBranch
    git pull origin $DefaultBranch
    git merge $currentBranch --no-ff -m "Merge branch '$currentBranch' for release $version"

    # 2. Crear tag
    Write-Host "  Creando tag $tag..." -ForegroundColor Cyan
    git tag -a $tag -m "Release $version"

    # 3. Push main + tag
    Write-Host "  Pusheando $DefaultBranch y tag $tag..." -ForegroundColor Cyan
    git push origin $DefaultBranch
    git push origin $tag

    # 4. Crear release en GitHub
    Write-Host "  Creando release en GitHub..." -ForegroundColor Cyan

    $body = @{
        tag_name = $tag
        name = "Release $version"
        body = "## Release $version`n`nVer el [CHANGELOG](https://github.com/$RepoOwner/$RepoName/blob/main/CHANGELOG.md) para más detalles."
        draft = $false
        prerelease = $false
        generate_release_notes = $true
    } | ConvertTo-Json

    $uri = "https://api.github.com/repos/$RepoOwner/$RepoName/releases"

    try {
        $response = Invoke-RestMethod -Uri $uri -Method Post -Headers $headers -Body $body -ContentType "application/json"
        Write-Host ""
        Write-Host "Release $tag publicada." -ForegroundColor Green
        Write-Host "URL: $($response.html_url)" -ForegroundColor Cyan
    }
    catch {
        Write-Host "Error al crear release en GitHub. El merge y tag ya se han pusheado." -ForegroundColor Red
        Write-Host "Puedes reintentar: .\release.ps1 publish" -ForegroundColor Yellow
        $errorMessage = $_.ErrorDetails.Message | ConvertFrom-Json -ErrorAction SilentlyContinue
        if ($errorMessage) {
            throw "Error: $($errorMessage.message)"
        }
        throw "Error: $_"
    }

    # 5. Volver a la rama original
    Write-Host ""
    Write-Host "  Volviendo a rama $currentBranch..." -ForegroundColor Cyan
    git checkout $currentBranch

    Write-Host ""
    Write-Host "Publicación completada." -ForegroundColor Green
    Write-Host ""
}

# --- Ejecución ---

switch ($Action) {
    "prepare" {
        Invoke-Prepare -Type $BumpType
    }
    "publish" {
        Invoke-Publish
    }
}
