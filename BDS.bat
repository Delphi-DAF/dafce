@echo off
setlocal enableextensions enabledelayedexpansion

set SELF_NAME=%~n0
set SELF_VS=4.1.0

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

if /i (%CMD%)==(help)  goto :HELP
if /i (%CMD%)==(init)  goto :INIT

if exist "%WORK_DIR%\%COMPUTERNAME%.project.cmd" (call "%WORK_DIR%\%COMPUTERNAME%.project.cmd" :on_init)

:: load project specific settings so we can override defaults
if  exist "%WORK_DIR%\project.cmd" (call "%WORK_DIR%\project.cmd" :on_init)

:: Determine build file: 1) --project arg, 2) PRJ_BUILD_FILE from project.cmd, 3) first .groupproj/.dproj found
if defined ARG_PROJECT (
  set "PRJ_BUILD_FILE=%ARG_PROJECT%"
  if not exist "!PRJ_BUILD_FILE!" set "PRJ_BUILD_FILE=%PRJ_DIR%\!ARG_PROJECT!"
)
if not defined PRJ_BUILD_FILE (
  for /R "%PRJ_DIR%" %%F in (*.groupproj) do (
    set "PRJ_BUILD_FILE=%%F"
    goto :found_prj
  )
  for /R "%PRJ_DIR%" %%F in (*.dproj) do (
    set "PRJ_BUILD_FILE=%%F"
    goto :found_prj
  )
)
:found_prj

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
) else (
  set "BDSbin=%BDS%\bin"
  set "rsvars=%BDS%\bin\\rsvars"
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
::       --project: <file> Project file to clean (.dproj or .groupproj)
::       --config: [Debug]|Release
::       --64: Build for 64-bit platform  
::       --output: <path> Set output directory
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
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  echo msbuild "%PRJ_BUILD_FILE%" /t:Clean /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG%
  call msbuild "%PRJ_BUILD_FILE%" /t:Clean /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG%
)
exit /B 0

::help>>
:: MAKE: Build project using MSBuild
::       --project: <file> Project file to build (.dproj or .groupproj)
::       --config: [Debug]|Release
::       --64: Build for 64-bit platform  
::       --output: <path> Set output directory
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
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  echo msbuild "%PRJ_BUILD_FILE%" /t:make /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG%
  call msbuild "%PRJ_BUILD_FILE%" /t:make /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG%
)
exit /B 0
  
::help>>
:: BUILD: Build project with full rebuild
::       --project: <file> Project file to build (.dproj or .groupproj)
::       --config: [Debug]|Release
::       --64: Build for 64-bit platform  
::       --output: <path> Set output directory
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
if exist "%BDSbin%" ( 
  call "%rsvars%" > nul
  echo msbuild "%PRJ_BUILD_FILE%" /t:Build /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG%
  call msbuild "%PRJ_BUILD_FILE%" /t:Build /verbosity:quiet /p:Platform=%ARG_PLATFORM% /p:Config=%ARG_CONFIG% 
)
exit /B 0

::help>>
:: START: Start RAD Studio IDE and load default project
::help<<
:START
start "BDS" "%BDSApp%" -idecaption="%PRJ_NAME%" -r"%PRJ_REG_KEY%" "%PRJ_BUILD_FILE%" 
exit /B 0

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
