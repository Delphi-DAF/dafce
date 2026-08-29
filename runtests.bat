@echo off
setlocal enableextensions enabledelayedexpansion

rem Run every spec runner declared by the specs group, on every platform asked for.
rem
rem   runtests [Win32^|Win64^|all]        default: all
rem
rem The list of suites is DERIVED from src\DAFSpecs.groupproj instead of written
rem here. A list kept by hand goes stale in the direction of running LESS, and a
rem suite that never runs looks exactly like a suite that passes: the previous
rem version of this file named seven of nineteen runners, one of them long gone.
rem
rem For the same reason a declared runner whose .exe is missing counts as a
rem FAILURE and not a skip. It was declared, so not finding it means the run is
rem incomplete -- and an incomplete measurement reported as success is worse than
rem no measurement, because it looks whole.

set "SELF_DIR=%~dp0"
if "%SELF_DIR:~-1%"=="\" set "SELF_DIR=%SELF_DIR:~0,-1%"
set "GROUP=%SELF_DIR%\src\DAFSpecs.groupproj"

set "PLATFORMS=Win32 Win64"
if /i "%~1"=="Win32" set "PLATFORMS=Win32"
if /i "%~1"=="Win64" set "PLATFORMS=Win64"
if /i "%~1"=="all" set "PLATFORMS=Win32 Win64"
if not "%~1"=="" if /i not "%~1"=="Win32" if /i not "%~1"=="Win64" if /i not "%~1"=="all" (
  echo ERROR: unknown argument [%~1]
  echo Usage: %~n0 [Win32^|Win64^|all]
  exit /B 2
)

if not exist "%GROUP%" (
  echo ERROR: specs group not found [%GROUP%]
  exit /B 2
)

rem Extract the project name from each <Projects Include="path\Name.dproj"> line
rem with plain string surgery. A for /f whose delimiter is a quote is the usual way
rem to do this and the usual way to get an empty list without an error.
set "SUITES="
set "COUNT=0"
for /f "usebackq delims=" %%L in (`findstr /C:"<Projects Include=" "%GROUP%"`) do (
  set "line=%%L"
  set "line=!line:*Include=!"
  set "line=!line:~2!"
  for /f "delims=>" %%A in ("!line!") do set "proj=%%A"
  set "proj=!proj:~0,-1!"
  for %%F in ("!proj!") do (
    set "SUITES=!SUITES! %%~nF"
    set /a COUNT+=1
  )
)

if !COUNT!==0 (
  echo ERROR: no suites derived from %GROUP% - nothing would have been run
  exit /B 2
)

set "PASS=0"
set "FAIL=0"
set "FAILURES="

echo ============================================================
echo  DAF Run Tests
echo  Suites: !COUNT! declared by src\DAFSpecs.groupproj
echo  Platforms: %PLATFORMS%
echo ============================================================

for %%P in (%PLATFORMS%) do (
  for %%S in (!SUITES!) do call :suite %%P %%S
)

echo.
echo ============================================================
echo  Results:  PASS=!PASS!  FAIL=!FAIL!  ^(!COUNT! suites x platforms^)
if !FAIL! gtr 0 (
  echo  Failed:!FAILURES!
  echo ============================================================
  exit /B 1
)
echo ============================================================
exit /B 0

:suite
set "_platform=%~1"
set "_name=%~2"
set "_exe=%SELF_DIR%\out\%_platform%\test\bin\%_name%.exe"
echo.
echo ---- %_platform%  %_name% ----
if not exist "!_exe!" (
  echo   MISSING: !_exe!
  set /a FAIL+=1
  set "FAILURES=!FAILURES! [%_platform%/%_name%:missing]"
  exit /B 0
)
call "!_exe!"
if !ERRORLEVEL! neq 0 (
  set /a FAIL+=1
  set "FAILURES=!FAILURES! [%_platform%/%_name%]"
) else (
  set /a PASS+=1
)
exit /B 0
