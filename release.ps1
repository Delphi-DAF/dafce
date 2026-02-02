1<#
.SYNOPSIS
    Script para gestionar versiones y releases de DAFce

.DESCRIPTION
    Permite incrementar la versión semántica y publicar releases en GitHub.

.PARAMETER Action
    La acción a realizar: bump o publish

.PARAMETER BumpType
    Tipo de incremento: major, minor o patch (solo para bump)

.EXAMPLE
    .\release.ps1 bump patch
    .\release.ps1 bump minor
    .\release.ps1 bump major
    .\release.ps1 publish
#>

param(
    [Parameter(Position=0)]
    [ValidateSet("bump", "publish", "help", "")]
    [string]$Action = "",

    [Parameter(Position=1)]
    [ValidateSet("major", "minor", "patch")]
    [string]$BumpType = "patch"
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
    Write-Host "  bump <tipo>   Incrementa la versión semántica"
    Write-Host "                Tipos: patch (default), minor, major"
    Write-Host "  publish       Crea tag y release en GitHub"
    Write-Host "  help          Muestra esta ayuda"
    Write-Host ""
    Write-Host "Ejemplos:" -ForegroundColor Yellow
    Write-Host "  .\release.ps1 bump patch    # 1.1.0 -> 1.1.1"
    Write-Host "  .\release.ps1 bump minor    # 1.1.0 -> 1.2.0"
    Write-Host "  .\release.ps1 bump major    # 1.1.0 -> 2.0.0"
    Write-Host "  .\release.ps1 publish       # Crea release en GitHub"
    Write-Host ""
    Write-Host "Flujo típico:" -ForegroundColor Yellow
    Write-Host "  1. .\release.ps1 bump patch"
    Write-Host "  2. git push"
    Write-Host "  3. .\release.ps1 publish"
    Write-Host ""
}

if ($Action -eq "" -or $Action -eq "help") {
    Show-Help
    exit 0
}

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$VersionFile = Join-Path $ScriptDir "src\VERSION.txt"
$EnvFile = Join-Path $ScriptDir ".env"
$RepoOwner = "Delphi-DAF"
$RepoName = "dafce"

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
    Write-Host "Versión actualizada a: $Version" -ForegroundColor Green
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

function Invoke-Bump {
    param([string]$Type)
    
    $currentVersion = Get-CurrentVersion
    $parts = $currentVersion.Split('.')
    $major = [int]$parts[0]
    $minor = [int]$parts[1]
    $patch = [int]$parts[2]
    
    switch ($Type) {
        "major" {
            $major++
            $minor = 0
            $patch = 0
        }
        "minor" {
            $minor++
            $patch = 0
        }
        "patch" {
            $patch++
        }
    }
    
    $newVersion = "$major.$minor.$patch"
    
    Write-Host "Versión actual: $currentVersion" -ForegroundColor Yellow
    Write-Host "Nueva versión:  $newVersion" -ForegroundColor Green
    
    Set-Version $newVersion
    
    # Commit del cambio de versión
    git add $VersionFile
    git commit -m "Bump version from $currentVersion to $newVersion"
    
    Write-Host ""
    Write-Host "Commit creado. Usa 'git push' para subir los cambios." -ForegroundColor Cyan
    Write-Host "Luego ejecuta '.\release.ps1 publish' para crear la release en GitHub." -ForegroundColor Cyan
    
    return $newVersion
}

function Invoke-Publish {
    $version = Get-CurrentVersion
    $tag = "v$version"
    $token = Get-GitHubToken
    
    Write-Host "Publicando release $tag..." -ForegroundColor Yellow
    
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
    
    # Verificar si el tag ya existe localmente
    $existingTag = git tag -l $tag
    if (-not $existingTag) {
        # Crear y pushear el tag
        Write-Host "Creando tag $tag..." -ForegroundColor Cyan
        git tag -a $tag -m "Release $version"
        git push origin $tag
    }
    else {
        Write-Host "Tag $tag ya existe, verificando en remoto..." -ForegroundColor Cyan
        # Verificar si está en remoto
        $remoteTag = git ls-remote --tags origin $tag
        if (-not $remoteTag) {
            git push origin $tag
        }
    }
    
    # Crear release en GitHub usando la API
    Write-Host "Creando release en GitHub..." -ForegroundColor Cyan
    
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
        Write-Host "¡Release $tag publicada exitosamente!" -ForegroundColor Green
        Write-Host "URL: $($response.html_url)" -ForegroundColor Cyan
    }
    catch {
        $errorMessage = $_.ErrorDetails.Message | ConvertFrom-Json -ErrorAction SilentlyContinue
        if ($errorMessage) {
            throw "Error al crear release: $($errorMessage.message)"
        }
        throw "Error al crear release: $_"
    }
}

# Ejecutar la acción solicitada
switch ($Action) {
    "bump" {
        Invoke-Bump -Type $BumpType
    }
    "publish" {
        Invoke-Publish
    }
}
