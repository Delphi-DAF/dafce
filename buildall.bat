@echo off
setlocal enableextensions enabledelayedexpansion

rem Build what this edition ships: framework, samples and specs, in that order,
rem for every platform asked for.
rem
rem   buildall [Win32^|Win64^|all] [--framework] [--samples] [--specs] [--clean]
rem
rem   Win32, Win64, all   the platforms; all by default
rem   --framework         the framework, in Debug and Release
rem   --samples           the samples
rem   --specs             the specs
rem   --clean             deletes out\ before building
rem
rem The scope options combine, and with none of them everything is built.
rem
rem Samples and specs are built only in Debug, which is the only config their optsets
rem describe, and they link the Debug framework whatever else was built. So
rem --samples and --specs build the Debug framework first, even without --framework:
rem it is an incremental make, and a framework already up to date costs nothing.
rem
rem The order is not cosmetic. Samples and specs link the framework's DCU and DCP,
rem so a framework left stale produces F2051 downstream -- and the suites of the
rem module you just changed stay green throughout, because they only link what
rem they need.
rem
rem --clean deletes out\ outright rather than calling BDS clean. Clean deletes what
rem the projects declare, so a DCU whose unit was renamed or retired belongs to no
rem project any more and survives it, still satisfying a uses clause nobody
rem removed. Only deleting the directory answers the question "does this build on a
rem machine that has never built it".
rem
rem Each step's scope is a .groupproj, so a project added to a group is built here
rem without anybody editing this file.

set "SELF_DIR=%~dp0"
if "%SELF_DIR:~-1%"=="\" set "SELF_DIR=%SELF_DIR:~0,-1%"

set "PLATFORMS=Win32 Win64"
set "DO_CLEAN="
set "DO_FRAMEWORK="
set "DO_SAMPLES="
set "DO_SPECS="
:parse_args
if "%~1"=="" goto :args_done
set "_arg=%~1"
if /i "%_arg%"=="Win32"       (set "PLATFORMS=Win32"       & goto :next_arg)
if /i "%_arg%"=="Win64"       (set "PLATFORMS=Win64"       & goto :next_arg)
if /i "%_arg%"=="all"         (set "PLATFORMS=Win32 Win64" & goto :next_arg)
if /i "%_arg%"=="--framework" (set "DO_FRAMEWORK=1"        & goto :next_arg)
if /i "%_arg%"=="--samples"   (set "DO_SAMPLES=1"          & goto :next_arg)
if /i "%_arg%"=="--specs"     (set "DO_SPECS=1"            & goto :next_arg)
if /i "%_arg%"=="--clean"     (set "DO_CLEAN=1"            & goto :next_arg)
echo ERROR: unknown argument [%~1]
echo Usage: %~n0 [Win32^|Win64^|all] [--framework] [--samples] [--specs] [--clean]
exit /B 2
:next_arg
shift
goto :parse_args
:args_done

if not defined DO_FRAMEWORK if not defined DO_SAMPLES if not defined DO_SPECS (
  set "DO_FRAMEWORK=1"
  set "DO_SAMPLES=1"
  set "DO_SPECS=1"
)

if defined DO_CLEAN (
  echo Removing "%SELF_DIR%\out" ...
  if exist "%SELF_DIR%\out" rmdir /s /q "%SELF_DIR%\out"
  if exist "%SELF_DIR%\out" (
    echo ERROR: could not remove "%SELF_DIR%\out"
    exit /B 2
  )
)

set "STEPS=0"
set "FAILED="
set "FW_CONFIGS=Debug  [linked by samples and specs]"
if defined DO_FRAMEWORK set "FW_CONFIGS=Debug+Release"

echo ============================================================
echo  DAF Build All
echo  Framework:     %PLATFORMS%  x  %FW_CONFIGS%
if defined DO_SAMPLES echo  Samples:       %PLATFORMS%  x  Debug
if defined DO_SPECS echo  Specs:         %PLATFORMS%  x  Debug
echo ============================================================

for %%P in (%PLATFORMS%) do (
  call :step %%P Debug   framework src/DAFGroup.groupproj
  if not errorlevel 1 if defined DO_FRAMEWORK call :step %%P Release framework src/DAFGroup.groupproj
  if not errorlevel 1 if defined DO_SAMPLES call :step %%P Debug   samples   src/DAFSamples.groupproj
  if not errorlevel 1 if defined DO_SPECS call :step %%P Debug   specs     src/DAFSpecs.groupproj
)

echo.
echo ============================================================
if defined FAILED (
  echo  BUILD FAILED:!FAILED!
  echo ============================================================
  exit /B 1
)
echo  BUILD OK - !STEPS! step^(s^)
echo ============================================================
exit /B 0

:step
set "_p=%~1"
set "_c=%~2"
set "_name=%~3"
set "_proj=%~4"
set "_flag="
if /i "%_p%"=="Win64" set "_flag=--64"
echo.
echo ---- %_p%  %_c%  %_name%  (%_proj%) ----
call "%SELF_DIR%\BDS.bat" make --project:%_proj% --config:%_c% %_flag%
if !ERRORLEVEL! neq 0 (
  set "FAILED=!FAILED! [%_p%/%_c%/%_name%]"
  exit /B 1
)
set /a STEPS+=1
exit /B 0
