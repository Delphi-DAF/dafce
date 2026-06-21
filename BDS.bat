@echo off
setlocal enableextensions enabledelayedexpansion

set SELF_NAME=%~n0
set SELF_VS=4.3.0

:: Análisis de argumentos
for %%A in (%*) do (
  set "arg=%%~A"
  set "firstChar=!arg:~0,1!"
  rem Primer argumento sin "-" se interpreta como el comando
  if not defined CMD (
    if not "!firstChar!"=="-" (
      set "CMD=!arg!"
    )
  ) 
  rem Argumento tipo --key=value
  if "!arg:~0,2!"=="--" (
    set "kvpair=!arg:~2!"
    for /f "tokens=1,* delims=:" %%K in ("!kvpair!") do (
      if "%%L"=="" (
        set "ARG_%%K=true"
      ) else (
        set "ARG_%%K=%%L"
      )
    )
  )
)
if not defined CMD set "CMD=start"

set "PRJ_ROOT_KEY=USR_PRJ"
:: Current dir without trailing backslash
set WORK_DIR=%~dp0
IF %WORK_DIR:~-1%==\ SET WORK_DIR=%WORK_DIR:~0,-1%


:: default PRJ vars
set "PRJ_DIR=%WORK_DIR%"
set "PRJ_VENDOR_DIR=%PRJ_DIR%\vendor"
set "PRJ_OUT_DIR=%PRJ_DIR%\out"

for %%i IN ("%PRJ_DIR%") DO set "PRJ_NAME=%%~ni"

set "PRJ_REG_KEY=%PRJ_ROOT_KEY%\%PRJ_NAME%"

:: load .env if present (KEY=VALUE lines; # comments and blank lines are ignored)
if exist "%WORK_DIR%\.env" (
  for /f "usebackq tokens=1,* delims== eol=#" %%K in ("%WORK_DIR%\.env") do (
    if not "%%K"=="" if not "%%L"=="" set "%%K=%%L"
  )
)

if /i (%CMD%)==(help)  goto :HELP
if /i (%CMD%)==(init)  goto :INIT

:: Default PRJ_BUILD_FILE: buscar primer .groupproj/.dproj (puede ser sobrescrito por project.cmd o --path:)
for /R "%PRJ_DIR%" %%F in (*.groupproj) do (
  set "PRJ_BUILD_FILE=%%F"
  goto :found_prj
)
for /R "%PRJ_DIR%" %%F in (*.dproj) do (
  set "PRJ_BUILD_FILE=%%F"
  goto :found_prj
)
:found_prj

if exist "%WORK_DIR%\%COMPUTERNAME%.project.cmd" (call "%WORK_DIR%\%COMPUTERNAME%.project.cmd" :on_init)

:: load project specific settings so we can override defaults
if exist "%WORK_DIR%\project.cmd" (call "%WORK_DIR%\project.cmd" :on_init)

:: Normalizar: --project: es alias de --path: (compatibilidad hacia atras)
if not defined ARG_path (
  if defined ARG_PROJECT set "ARG_path=!ARG_PROJECT!"
)

:: --path: tiene prioridad final (excepto para exec)
if defined ARG_path (
  if /i not "!CMD!"=="exec" (
    set "PRJ_BUILD_FILE=!ARG_path!"
    if not exist "!PRJ_BUILD_FILE!" set "PRJ_BUILD_FILE=%PRJ_DIR%\!ARG_path!"
  )
)

:: add vendor_bin to path, so you can install your vendor pkg
set PATH=%PATH%;%PRJ_VENDOR_DIR%\bin

:: create output dir
mkdir "%PRJ_OUT_DIR%" 2>nul

if (%PRJ_IDE%)==() goto :ERR_PRJ_IDE_NOT_DEFINED
:: select ide version from PRJ_IDE: remove last digit & remove D prefix
SET IDE_VER=%PRJ_IDE:~0,-1%
SET IDE_VER=%IDE_VER:~1%

:: find delphi registry key
set "delphi_reg="
if %IDE_VER% LEQ 7 (
  set "delphi_brand=Borland"
  set "delphi_brand_base=Delphi"
) else (
  if !IDE_VER! LEQ 14 (
    set /A IDE_VER-=6
    set "delphi_brand=Borland"
    set "delphi_brand_base=BDS"
  ) else (
    set "delphi_brand=Embarcadero"
    set "delphi_brand_base=BDS"
    if !IDE_VER! LEQ 19 (
      set /A IDE_VER-=7
    ) else (
      if !IDE_VER! LEQ 29 (
        set /A IDE_VER-=6
      )
    )
  )
)
SET "IDE_VER=%IDE_VER%.0"
set "BDS_APP_REG_KEY=App"
if (%ARG_64%)==(true) (
echo Using 64-bit IDE
set "BDS_APP_REG_KEY=App x64"
shift /1
)


if (%delphi_brand%)==() goto :ERR_IDE_VS_NOT_FOUND
set "delphi_reg=HKCU\Software\%delphi_brand%\%delphi_brand_base%\!IDE_VER!"
:: Get BDS root path & BDS app from Windows Registry
for /f "skip=2 tokens=2,*" %%a in ('reg query "%delphi_reg%" /v "RootDir" 2^>nul') do set "BDS=%%~b"

if (%ARG_64%)==(true) (
  for /f "skip=2 tokens=3,*" %%a in ('reg query "%delphi_reg%" /v "%BDS_APP_REG_KEY%" 2^>nul') do set "BDSApp=%%~b"
) else (
  for /f "skip=2 tokens=2,*" %%a in ('reg query "%delphi_reg%" /v "%BDS_APP_REG_KEY%" 2^>nul') do set "BDSApp=%%~b"
)
set BDS=!BDS:~0,-1!

if ("%BDSApp%")==("") goto :ERR_IDE_VS_NOT_FOUND
set "delphi_reg=HKCU\Software\%delphi_brand%\%PRJ_REG_KEY%\!IDE_VER!"
if (%ARG_64%)==(true) (
  set "BDSbin=%BDS%\bin64"
  set "rsvars=%BDS%\bin64\rsvars64"
  set "ARG_PLATFORM=Win64"
) else (
  set "BDSbin=%BDS%\bin"
  set "rsvars=%BDS%\bin\\rsvars"
  set "ARG_PLATFORM=Win32"
)
:: clear aux variables
set "delphi_brand="
set "delphi_brand_base="
set "BDS_APP_REG_KEY="

:: set IDE DefaultProjectsDirectory 
reg add "%delphi_reg%\Globals" /v "DefaultProjectsDirectory" /t REG_SZ /d "%PRJ_DIR%" /f >nul

call "%rsvars%" > nul
call "%WORK_DIR%\project.cmd" :on_start
call :BANNER

:: select command
call :%CMD% %*
exit /B 0
goto :eof

:: Error handling
:ERR_PRJ_IDE_NOT_DEFINED
echo ERROR: PRJ_IDE variable not defined
echo.
echo The PRJ_IDE variable must be set in project.cmd to specify the RAD Studio version.
echo.
echo Example values:
echo   set "PRJ_IDE=D270"  for RAD Studio 11.0 Alexandria 
echo   set "PRJ_IDE=D280"  for RAD Studio 11.3 Alexandria 
echo   set "PRJ_IDE=D290"  for RAD Studio 12.0 Athens
echo   set "PRJ_IDE=D370"  for RAD Studio 13.0 Florence
echo etc.
echo.
echo Please use INIT command to create a valid project.cmd (or edit it manually) and add the appropriate PRJ_IDE setting.
goto :eof

:ERR_IDE_VS_NOT_FOUND
echo ERROR: RAD Studio IDE version !IDE_VER! not found
echo.
echo The specified IDE version is not installed or not properly configured.
echo.
echo Please check:
echo   1. RAD Studio !IDE_VER! is installed
echo   2. PRJ_IDE variable in project.cmd matches your installed version
echo   3. Registry entries are properly configured
echo.
echo Current PRJ_IDE setting: %PRJ_IDE%
goto :eof

:CREATE_PROJECT_CMD
echo Creating default project.cmd file...
(
echo goto %%1
echo.
echo :: Called early to set PRJ_IDE and basic variables
echo :on_init
echo :: mandatory project vars
echo set "PRJ_IDE=D370"
echo.
echo :: custom project vars

echo goto :eof
echo.
echo :: Called after IDE detection to set final variables
echo :on_start
echo :: here you can assume BDS defaultvariables like BDS_INCLUDE are available
echo goto :eof
) > "%WORK_DIR%\project.cmd"
echo Default project.cmd created. You may need to edit PRJ_IDE for your RAD Studio version.
goto :eof

:: Available commands
::-------------------------------

:: Show version banner
:BANNER
echo %SELF_NAME% v%SELF_VS% - Project build and management tool for EmbarcaderoRAD Studio
echo -- (c) j.cangas@pm.me ^| Use %SELF_NAME% help to see available commands --
exit /B 0

:: to inspect command line arguments
:DEBUG
echo Command:
echo %CMD%
echo Args:
SET ARG_
exit /B 0

::help>>
:: HELP: Show this help
::       Usage: help [COMMAND]
::help<<
:HELP
call :BANNER
echo.
echo ======= Help =======
if "%~2"=="" (
echo Syntax: %SELF_NAME% [command] [--key:value] [--flag]
echo        %SELF_NAME% help [COMMAND]
echo.
echo CONFIGURATION:
echo This script requires a project.cmd file with project-specific settings. See INIT command to create a default one.
echo Optional machine-specific overrides can be placed in "%%COMPUTERNAME%%.project.cmd"
echo.
echo COMMANDS:
)
set "inHelp="
if "%~2"=="" (set "showBlock=yes") else (set "showBlock=")
for /f "usebackq delims=" %%i in (`findstr /r /c:"^::help>>" /c:"^::help<<" /c:"^::" "%~f0"`) do (
    set "line=%%i"
    if "!line!"=="::help>>" (
        set "inHelp=yes"
        if not "%~2"=="" set "showBlock="
    ) else if "!line!"=="::help<<" (
        if not "%~2"=="" if defined showBlock goto :help_done
        set "inHelp="
        if defined showBlock echo.
    ) else if defined inHelp (
        if "!line:~0,2!"=="::" (
            set "helpText=!line:~3!"
            if not "%~2"=="" (
                for /f "tokens=1 delims=:" %%c in ("!helpText!") do (
                    if /i "%%c"=="%~2" set "showBlock=yes"
                )
            )
            if defined showBlock echo !helpText!
        )
    )
)
:help_done
if "%~2"=="" (
echo EXAMPLES:
echo   %SELF_NAME% build --64 --config:Release
echo   %SELF_NAME% clean --config:Debug
echo   %SELF_NAME% exec --path:out\Win64\test\bin\BeanMemoryTests.exe --64
echo   %SELF_NAME% help init
)
echo ---------------------------
goto :eof

::help>>
:: INIT: Create a default project.cmd file
::help<<
:INIT

if exist "%WORK_DIR%\project.cmd" (
    echo project.cmd already exists. Use --force to overwrite.
    if not "%ARG_FORCE%"=="true" goto :eof
)
echo in iinit
call :CREATE_PROJECT_CMD
echo.
echo project.cmd created successfully.
echo Edit the PRJ_IDE variable to match your RAD Studio version.
goto :eof

::help>>
:: INSPECT: Show project and BDS variables
::help<<
:INSPECT
echo.
echo    project variables
echo ---------------------------
set PRJ_
echo.
echo    BDS variables
echo ---------------------------
SET BDS
echo delphi_reg=%delphi_reg%
exit /B 0

:: Create installer using Inno Setup
::help>>
:: INNO: Create installer using Inno Setup
::help<<
:INNO
iscc /Qp .\AppSetup.iss /DBuildConfig=Release
exit /B 0

::help>>
:: CLEAN: Clean build artifacts
::       --path: <file>    Project file to clean (.cbproj or .groupproj)
::       --config: [Debug]|Release
::       --64: Build for 64-bit platform
::       --output: <path>  Set output directory
::       --verbose: <level> MSBuild verbosity: quiet|minimal|normal|detailed|diagnostic (default: quiet)
::help<<
:CLEAN
if (%ARG_CONFIG%)==() (
  set "ARG_CONFIG=Debug"
) 
if (%ARG_64%)==(true) (
  set "ARG_PLATFORM=Win64"
) else  (
  set "ARG_PLATFORM=Win32"
)

if (%ARG_OUTPUT%) neq () (
  set "PRJ_OUT_DIR=%ARG_OUTPUT%"
)
set "MSB_VERBOSITY=quiet"
if defined ARG_verbose set "MSB_VERBOSITY=%ARG_verbose%"
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  call msbuild "%PRJ_BUILD_FILE%" /t:Clean /verbosity:%MSB_VERBOSITY% /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG% /m:1
  if !ERRORLEVEL!==0 echo Clean succeeded: %ARG_PLATFORM% %ARG_CONFIG%
)
exit /B 0

::help>>
:: MAKE: Build project using MSBuild (incremental)
::       --path: <file>    Project file to build (.cbproj or .groupproj)
::       --config: [Debug]|Release
::       --64: Build for 64-bit platform
::       --output: <path>  Set output directory
::       --verbose: <level> MSBuild verbosity: quiet|minimal|normal|detailed|diagnostic (default: quiet)
::help<<
:MAKE
if (%ARG_CONFIG%)==() (
  set "ARG_CONFIG=Debug"
) 
if (%ARG_64%)==(true) (
  set "ARG_PLATFORM=Win64"
) else  (
  set "ARG_PLATFORM=Win32"
)

if (%ARG_OUTPUT%) neq () (
  set "PRJ_OUT_DIR=%ARG_OUTPUT%"
)
set "MSB_VERBOSITY=quiet"
if defined ARG_verbose set "MSB_VERBOSITY=%ARG_verbose%"
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  call msbuild "%PRJ_BUILD_FILE%" /t:make /verbosity:%MSB_VERBOSITY% /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG% /m:1
  if !ERRORLEVEL!==0 echo Make succeeded: %ARG_PLATFORM% %ARG_CONFIG%
)
exit /B 0
  
::help>>
:: BUILD: Build project with full rebuild
::        --path: <file>    Project file to build (.cbproj or .groupproj)
::        --config: [Debug]|Release
::        --64: Build for 64-bit platform
::        --output: <path>  Set output directory
::        --verbose: <level> MSBuild verbosity: quiet|minimal|normal|detailed|diagnostic (default: quiet)
::help<<
:BUILD
if (%ARG_CONFIG%)==() (
  set "ARG_CONFIG=Debug"
) 
if (%ARG_64%)==(true) (
  set "ARG_PLATFORM=Win64"
) else  (
  set "ARG_PLATFORM=Win32"
)

if (%ARG_OUTPUT%) neq () (
  set "PRJ_OUT_DIR=%ARG_OUTPUT%"
)
set "MSB_VERBOSITY=quiet"
if defined ARG_verbose set "MSB_VERBOSITY=%ARG_verbose%"
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  call msbuild "%PRJ_BUILD_FILE%" /t:Build /verbosity:%MSB_VERBOSITY% /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG% /m:1
  if !ERRORLEVEL!==0 echo Build succeeded: %ARG_PLATFORM% %ARG_CONFIG%
)
exit /B 0

::help>>
:: EXEC: Execute an .exe or .bat with the full project environment active
::       (rsvars, project.cmd :on_start and .env already loaded)
::       --path: <file>  path to .exe or .bat to execute (relative to project root or absolute)
::help<<
:EXEC
if not defined ARG_path (
  echo ERROR: --path: argument is required
  echo Usage: %SELF_NAME% exec --path:^<file^>
  exit /B 1
)
set "_exec_target=%ARG_path%"
if not exist "!_exec_target!" set "_exec_target=%PRJ_DIR%\!ARG_path!"
if not exist "!_exec_target!" (
  echo ERROR: File not found: %ARG_path%
  exit /B 1
)
:: auto-detect platform from path (Win64/Win32) if not set by --64 flag
set "_exec_platform=%ARG_PLATFORM%"
echo !_exec_target! | findstr /i "\\Win64\\" >nul && set "_exec_platform=Win64"
echo !_exec_target! | findstr /i "\\Win32\\" >nul && set "_exec_platform=Win32"
:: add project bin output to PATH so runtime DLLs/BPLs are found
set "PATH=%PRJ_OUT_DIR%\%_exec_platform%\bin;%PATH%"
set "_exec_platform="
call "!_exec_target!"
exit /B %ERRORLEVEL%

::help>>
:: START: Start RAD Studio IDE and load default project
::help<<
:START
start "BDS" "%BDSApp%" -idecaption="%PRJ_NAME%" -r"%PRJ_REG_KEY%" "%PRJ_BUILD_FILE%" 
exit /B 0

::help>>
:: AUTOBUILD: Build all projects in a .groupproj sequentially with ICE-retry logic
::            For each project: runs a full Build to regenerate PCH, then retries
::            with incremental Make if an Internal Compiler Error (ICE) is detected.
::            Stops immediately if a real (non-ICE) compile error is found.
::       --path:     <file>    .groupproj file to process (required)
::       --config:   [Debug]|Release
::       --64:                 Build for Win64 (default Win32)
::       --jobs:     [1]|N     MSBuild parallelism (default 1, recommended to avoid ICE)
::       --maxretry: [15]      Max make retries per project on ICE
::       --step:     [1]       Start from this step number (skip earlier projects)
::       --verbose:  [minimal] MSBuild verbosity: quiet|minimal|normal|detailed|diagnostic
::       --dryrun:             Print what would be done without invoking MSBuild
::       --makeonly:           Skip initial Build, run only Make (noop if already up-to-date)
::help<<
:AUTOBUILD
if (%ARG_CONFIG%)==() set "ARG_CONFIG=Debug"
if (%ARG_64%)==(true) (
  set "ARG_PLATFORM=Win64"
) else (
  set "ARG_PLATFORM=Win32"
)
set "MSB_VERBOSITY=minimal"
if defined ARG_verbose set "MSB_VERBOSITY=%ARG_verbose%"
set "AB_JOBS=1"
if defined ARG_jobs set "AB_JOBS=%ARG_jobs%"
set "AB_MAXRETRY=15"
if defined ARG_maxretry set "AB_MAXRETRY=%ARG_maxretry%"
set "AB_START_STEP=1"
if defined ARG_step set "AB_START_STEP=%ARG_step%"
set "AB_TARGET=Build"
if /i "%ARG_makeonly%"=="true" set "AB_TARGET=Make"
set "AB_DRYRUN=false"
if /i "%ARG_dryrun%"=="true" set "AB_DRYRUN=true"

rem PRJ_BUILD_FILE is already resolved by BDS.bat startup logic; normalize to absolute
set "AB_GROUPPROJ=%PRJ_BUILD_FILE%"
for %%G in ("!AB_GROUPPROJ!") do set "AB_GROUPPROJ=%%~fG"
if not exist "!AB_GROUPPROJ!" (
  echo AUTOBUILD ERROR: groupproj not found: %PRJ_BUILD_FILE%
  exit /B 1
)
rem Get the directory containing the groupproj (no trailing backslash)
for %%D in ("!AB_GROUPPROJ!") do set "AB_GROUPPROJ_DIR=%%~dpD"
if "!AB_GROUPPROJ_DIR:~-1!"=="\" set "AB_GROUPPROJ_DIR=!AB_GROUPPROJ_DIR:~0,-1!"

rem --- Collect project list from groupproj (<Projects Include="..."> lines only) ---
set "AB_PROJ_LIST_FILE=%TEMP%\_bds_autobuild_%RANDOM%.tmp"
if exist "%AB_PROJ_LIST_FILE%" del "%AB_PROJ_LIST_FILE%"
findstr /i "Projects Include=" "!AB_GROUPPROJ!" > "%AB_PROJ_LIST_FILE%"
rem Post-filter: parse each line and emit only valid .cbproj relative paths
set "AB_PROJ_CBPROJ_FILE=%TEMP%\_bds_autobuild_cb_%RANDOM%.tmp"
if exist "%AB_PROJ_CBPROJ_FILE%" del "%AB_PROJ_CBPROJ_FILE%"
for /f "usebackq tokens=*" %%L in ("%AB_PROJ_LIST_FILE%") do (
  set "_ab_line=%%L"
  set "_ab_line=!_ab_line:*Include=!"
  set "_ab_line=!_ab_line:~2!"
  for /f "tokens=1 delims=> " %%P in ("!_ab_line!") do set "_ab_cbproj=%%P"
  set "_ab_cbproj=!_ab_cbproj:"=!"
  if "!_ab_cbproj:~-7!"==".cbproj" echo !_ab_cbproj!>> "%AB_PROJ_CBPROJ_FILE%"
)
del "%AB_PROJ_LIST_FILE%" 2>nul

rem --- Count total projects by iterating the list file ---
set "AB_TOTAL=0"
for /f "usebackq tokens=*" %%F in ("%AB_PROJ_CBPROJ_FILE%") do set /a AB_TOTAL+=1

echo.
echo ==========================================
echo  AUTOBUILD  %ARG_PLATFORM% %ARG_CONFIG%
echo  Group: !AB_GROUPPROJ!
echo  Projects: !AB_TOTAL!  MaxRetry: !AB_MAXRETRY!  Jobs: !AB_JOBS!  StartStep: !AB_START_STEP!  Target: !AB_TARGET!  DryRun: !AB_DRYRUN!
echo ==========================================

set "AB_INDEX=0"
set "AB_FAILED="
set "AB_LOG=%TEMP%\_bds_autobuild_%RANDOM%.log"

for /f "usebackq tokens=*" %%F in ("%AB_PROJ_CBPROJ_FILE%") do (
  if not defined AB_FAILED (
    set /a AB_INDEX+=1
    rem Skip steps before AB_START_STEP
    if !AB_INDEX! lss !AB_START_STEP! (
      echo  [!AB_INDEX!/!AB_TOTAL!] SKIP ^(--step:!AB_START_STEP!^)
    ) else (
    set "_ab_rel=%%F"
    set "_ab_cbproj=!AB_GROUPPROJ_DIR!\!_ab_rel!"
    for /f "tokens=1-3 delims=:." %%H in ("!TIME: =0!") do set "_ab_time=%%H:%%I:%%J"
    echo.
    echo ------------------------------------------
    echo  [!AB_INDEX!/!AB_TOTAL! !_ab_time!] !_ab_rel!
    echo ------------------------------------------

    rem === Run MSBuild: /t:Build (default) or /t:Make (--makeonly) ===
    echo  running !AB_TARGET! ...
    if exist "!AB_LOG!" del "!AB_LOG!"
    if "!AB_DRYRUN!"=="true" (
      echo  [DRY-RUN] msbuild "!_ab_cbproj!" /t:!AB_TARGET! /p:Platform=!ARG_PLATFORM! /p:Config=!ARG_CONFIG!
      echo  OK: !AB_TARGET! succeeded ^(dry-run^)
    ) else (
      call msbuild "!_ab_cbproj!" /t:!AB_TARGET! /m:!AB_JOBS! /verbosity:!MSB_VERBOSITY! /p:Platform=!ARG_PLATFORM! /p:Config=!ARG_CONFIG! /nologo >> "!AB_LOG!" 2>&1
      if !ERRORLEVEL!==0 (
        echo  OK: !AB_TARGET! succeeded
      ) else (
        findstr /i "INTERNAL COMPILER ERROR\|error in backend\|unable to execute command\|fatal error: error in backend" "!AB_LOG!" >nul 2>&1
        if !ERRORLEVEL!==0 (
          echo  ICE detected - starting make-retry loop ...
          call :AB_RETRY_LOOP "!_ab_cbproj!" "!_ab_rel!"
        ) else (
          echo  REAL ERROR in [!_ab_rel!] - aborting autobuild
          echo  --- last lines of build log ---
          more /E +0 "!AB_LOG!" 2>nul
          set "AB_FAILED=!_ab_rel!"
        )
      )
    )
    )
  )
)

del "%AB_PROJ_CBPROJ_FILE%" 2>nul
if exist "!AB_LOG!" del "!AB_LOG!" 2>nul

if not "!AB_FAILED!"=="" (
  echo.
  echo ==========================================
  echo  AUTOBUILD FAILED at: !AB_FAILED!
  echo ==========================================
  exit /B 1
) else (
  echo.
  echo ==========================================
  echo  AUTOBUILD COMPLETE - !AB_INDEX! projects OK
  echo ==========================================
  exit /B 0
)

rem --- Retry loop subroutine: called with cbproj path and display name ---
:AB_RETRY_LOOP
set "_arl_proj=%~1"
set "_arl_name=%~2"
set "_arl_retry=0"
set "_arl_log=%TEMP%\_bds_arl_%RANDOM%.log"

:AB_RETRY_LOOP_NEXT
rem Capture newest file in lib output dir before make (progress marker)
set "_arl_newest_before="
for /f "usebackq tokens=*" %%N in (`dir /s /b /O:-D "%PRJ_OUT_DIR%\%ARG_PLATFORM%\lib" 2^>nul`) do (
  if not defined _arl_newest_before set "_arl_newest_before=%%N"
)

set /a _arl_retry+=1
echo  [Retry !_arl_retry!/!AB_MAXRETRY!] make ...
if exist "!_arl_log!" del "!_arl_log!"
if "!AB_DRYRUN!"=="true" (
  echo  [DRY-RUN] msbuild "!_arl_proj!" /t:Make /p:Platform=!ARG_PLATFORM! /p:Config=!ARG_CONFIG!
  echo  OK: make succeeded ^(dry-run^)
  if exist "!_arl_log!" del "!_arl_log!" 2>nul
  exit /B 0
)
call msbuild "!_arl_proj!" /t:Make /m:!AB_JOBS! /verbosity:!MSB_VERBOSITY! /p:Platform=!ARG_PLATFORM! /p:Config=!ARG_CONFIG! /nologo >> "!_arl_log!" 2>&1
if !ERRORLEVEL!==0 (
  echo  OK: make succeeded after !_arl_retry! retries
  if exist "!_arl_log!" del "!_arl_log!" 2>nul
  exit /B 0
)

rem Still failing - check if ICE
findstr /i "INTERNAL COMPILER ERROR" "!_arl_log!" >nul 2>&1
if !ERRORLEVEL! neq 0 (
  echo  REAL ERROR appeared during retry [!_arl_name!] - aborting
  more /E +0 "!_arl_log!" 2>nul
  if exist "!_arl_log!" del "!_arl_log!" 2>nul
  set "AB_FAILED=!_arl_name!"
  exit /B 1
)

rem Still ICE - check for progress
set "_arl_newest_after="
for /f "usebackq tokens=*" %%N in (`dir /s /b /O:-D "%PRJ_OUT_DIR%\%ARG_PLATFORM%\lib" 2^>nul`) do (
  if not defined _arl_newest_after set "_arl_newest_after=%%N"
)

if "!_arl_newest_before!"=="!_arl_newest_after!" (
  echo  STUCK: no new output after retry !_arl_retry! - aborting [!_arl_name!]
  if exist "!_arl_log!" del "!_arl_log!" 2>nul
  set "AB_FAILED=!_arl_name! (stuck/no-progress)"
  exit /B 1
)

if !_arl_retry! geq !AB_MAXRETRY! (
  echo  MAX RETRIES ^(!AB_MAXRETRY!^) exceeded for [!_arl_name!] - aborting
  if exist "!_arl_log!" del "!_arl_log!" 2>nul
  set "AB_FAILED=!_arl_name! (max-retry)"
  exit /B 1
)

echo  ICE again but progress detected - continuing ...
goto :AB_RETRY_LOOP_NEXT

::help>>
:: ENV: Setup development environment
::     --mode: [deve]|prod|test
::help<<
:ENV
if (%ARG_MODE%)==() (
  set "ARG_MODE=deve"
) 

set /p CONFIRM=setup env to "%ARG_MODE%" in "%PRJ_OUT_DIR%" (y/n)?: 
if /i "%CONFIRM%"=="y" (
  rmdir /s /q "%PRJ_OUT_DIR%"
  robocopy "%PRJ_DIR%\src\runenv\_shared" "%PRJ_OUT_DIR%" /E /NJH /NJS /NFL /NP /NDL
  robocopy "%PRJ_DIR%\src\runenv\%ARG_MODE%" "%PRJ_OUT_DIR%" /E /NJH /NJS /NFL /NP /NDL
  set "DAF_APP_ENV=%ARG_MODE%"
  echo environment established to %ARG_MODE%
) else (
  echo canceled
)
exit /B 0
