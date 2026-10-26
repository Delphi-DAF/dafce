goto %1

:: Called early to set PRJ_IDE and basic variables
:on_init
:: mandatory project vars
:: a PRJ_IDE from the environment or .env is kept; %COMPUTERNAME%.project.cmd,
:: called after this file, overrides it
if not defined PRJ_IDE set "PRJ_IDE=D370"

:: custom project vars
:: BDS.bat keeps each folder's IDE profile under HKCU\Software\Embarcadero\DAF, one key per folder name
set "PRJ_ROOT_KEY=DAF"

set "PRJ_BUILD_FILE=%PRJ_DIR%\src\DAFGroup.groupproj"
SET "PRJ_LOAD=%PRJ_DIR%\src\DAFGroup.groupproj"
set "DAF_CONTENTROOT=%PRJ_OUT_DIR%"
set "DAF_ENVIRONMENT=Development"


goto :eof

:: Called after IDE detection to set final variables
:on_start
:: here you can assume BDS defaultvariables like BDS_INCLUDE are available
goto :eof
