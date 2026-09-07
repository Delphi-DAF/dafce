goto %1

:: Called early to set PRJ_IDE and basic variables
:on_init
:: mandatory project vars
set "PRJ_IDE=D370"

:: custom project vars
set "PRJ_ROOT_KEY=BDSDEFAULTS"

set "PRJ_BUILD_FILE=%PRJ_DIR%\src\DAFGroup.groupproj"
SET "PRJ_LOAD=%PRJ_DIR%\src\DAFGroup.groupproj"
set "DAF_CONTENT_ROOT=%PRJ_OUT_DIR%"
set "DAF_APP_ENV=Development"


goto :eof

:: Called after IDE detection to set final variables
:on_start
:: here you can assume BDS defaultvariables like BDS_INCLUDE are available
goto :eof
