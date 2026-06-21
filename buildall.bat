@echo off
setlocal enableextensions enabledelayedexpansion

set "SELF_DIR=%~dp0"
if "%SELF_DIR:~-1%"=="\" set "SELF_DIR=%SELF_DIR:~0,-1%"
set "BDS=%SELF_DIR%\BDS.bat"

echo ============================================================
echo  DAF Build All
echo  Framework:     Win32+Win64  x  Debug+Release
echo  Samples+Tests: Win32+Win64  x  Debug
echo ============================================================

call :step "Framework  Win32  Debug"    --path:src\DAFGroup.groupproj    --config:Debug        || goto :failed
call :step "Framework  Win32  Release"  --path:src\DAFGroup.groupproj    --config:Release      || goto :failed
call :step "Framework  Win64  Debug"    --path:src\DAFGroup.groupproj    --config:Debug    --64 || goto :failed
call :step "Framework  Win64  Release"  --path:src\DAFGroup.groupproj    --config:Release  --64 || goto :failed
call :step "Samples    Win32  Debug"    --path:src\DAFSamples.groupproj  --config:Debug        || goto :failed
call :step "Samples    Win64  Debug"    --path:src\DAFSamples.groupproj  --config:Debug    --64 || goto :failed
call :step "Tests      Win32  Debug"    --path:src\DAFTestGroup.groupproj --config:Debug        || goto :failed
call :step "Tests      Win64  Debug"    --path:src\DAFTestGroup.groupproj --config:Debug    --64 || goto :failed

echo.
echo ============================================================
echo  Build All COMPLETE
echo ============================================================
exit /B 0

:failed
exit /B 1

:step
set "_lbl=%~1"
echo.
echo ---- %_lbl% ----
call "%BDS%" make %~2 %~3 %~4 %~5
if !ERRORLEVEL! neq 0 (
    echo.
    echo ============================================================
    echo  FAILED: %_lbl%
    echo ============================================================
    exit /B 1
)
exit /B 0
