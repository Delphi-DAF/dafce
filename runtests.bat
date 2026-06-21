@echo off
setlocal enableextensions enabledelayedexpansion

set "SELF_DIR=%~dp0"
if "%SELF_DIR:~-1%"=="\" set "SELF_DIR=%SELF_DIR:~0,-1%"
set "OUT=%SELF_DIR%\out\test\bin"

set "PASS=0"
set "FAIL=0"
set "SKIP=0"
set "FAILURES="

echo ============================================================
echo  DAF Run Tests
echo  Win32 + Win64
echo ============================================================

call :suite Win32 MiniSpecSpecs
call :suite Win32 CommonsSpecs
call :suite Win32 ConfigurationSpecs
call :suite Win32 DISpecs
call :suite Win32 MediatRSpecs
call :suite Win32 SQLCuteSpecs
call :suite Win32 SQLCuteCompilersSpecs

call :suite Win64 MiniSpecSpecs
call :suite Win64 CommonsSpecs
call :suite Win64 ConfigurationSpecs
call :suite Win64 DISpecs
call :suite Win64 MediatRSpecs
call :suite Win64 SQLCuteSpecs
call :suite Win64 SQLCuteCompilersSpecs

echo.
echo ============================================================
echo  Results:  PASS=!PASS!  FAIL=!FAIL!  SKIP=!SKIP!
if !FAIL! gtr 0 (
    echo  Failed suites:!FAILURES!
    echo ============================================================
    exit /B 1
)
echo ============================================================
exit /B 0

:suite
set "_platform=%~1"
set "_name=%~2"
set "_exe=%OUT%\%_platform%\%_name%.exe"
echo.
echo ---- %_platform%  %_name% ----
if not exist "!_exe!" (
    echo   SKIP: !_exe! not found
    set /a SKIP+=1
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
