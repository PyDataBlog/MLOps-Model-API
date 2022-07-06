@ECHO OFF
TITLE Samp project compiler...

REM Then file name and path to be compiled
SET FILENAME=alar
SET FILEPATH=%~dp0

REM -d<num>  Debugging level (default = -d1)
REM     0    No symbolic information, no run-time checks
REM     1    Run-time checks, no symbolic information
REM     2    Full debug information and dynamic checking
REM     3    Same as -d2, but implies -O0
SET DLEVEL=-d3

REM -O<num>  Optimization level (default = -O1)
REM     0    No optimization
REM     1    JIT-compatible optimizations only
REM     2    Full optimizations
SET OLEVEL=-O1

SET FAMX="%FILEPATH%\%FILENAME%.amx"
IF EXIST %FAMX% (
  ECHO Info: 'AMX' file has been deleted from folder
  DEL %FAMX%
  TIMEOUT /T 2 > NUL
)

SET FXML="%FILEPATH%\%FILENAME%.xml"
IF EXIST %FXML% (
  ECHO Info: 'XML' file has been deleted from folder
  DEL %FXML%
  TIMEOUT /T 2 > NUL
)

ECHO.
"%FILEPATH%/../pawno/pawncc.exe" "-r" "%FILENAME%.pwn" -i"./pawno/include" "-;" "-(" "%DLEVEL%" "%OLEVEL%"

:GPAUSE
  PAUSE
:GEXIT
  EXIT
