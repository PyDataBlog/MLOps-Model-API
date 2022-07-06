for /d %%g IN (*) DO (
set CSTM=%%~g
set /A ADDTO=0
call :injext
if "%ADDTO%" GEQ "2" set FNDMORE=1
)
exit /b
:injext
call "%GBC%\CreateGamSubDir.bat"
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%CSTM%"') do (
if /I "%%~xa"==".gz" "%GBC%\fart.exe" "*.bat" [EXT] gz
if /I "%%~xa"==".dsk" "%GBC%\fart.exe" "*.bat" [EXT] dsk
if /I "%%~xa"==".sad" "%GBC%\fart.exe" "*.bat" [EXT] sad
if /I "%%~xa"==".mgt" "%GBC%\fart.exe" "*.bat" [EXT] mgt
if /I "%%~xa"==".sdf" "%GBC%\fart.exe" "*.bat" [EXT] sdf
if /I "%%~xa"==".td0" "%GBC%\fart.exe" "*.bat" [EXT] td0
if /I "%%~xa"==".sbt" "%GBC%\fart.exe" "*.bat" [EXT] sbt
if /I "%%~xa"==".cbm" "%GBC%\fart.exe" "*.bat" [EXT] cbm
if /I "%%~xa"==".zip" "%GBC%\fart.exe" "*.bat" [EXT] zip
if /I "%%~xa"==".gz" set SAMCROM=%%~a
if /I "%%~xa"==".dsk" set SAMCROM=%%~a
if /I "%%~xa"==".sad" set SAMCROM=%%~a
if /I "%%~xa"==".mgt" set SAMCROM=%%~a
if /I "%%~xa"==".sdf" set SAMCROM=%%~a
if /I "%%~xa"==".td0" set SAMCROM=%%~a
if /I "%%~xa"==".sbt" set SAMCROM=%%~a
if /I "%%~xa"==".cbm" set SAMCROM=%%~a
if /I "%%~xa"==".zip" set SAMCROM=%%~a
if /i "%%~xa"==".bak" exit /b
if /i "%%~xa"==".bat" exit /b
REM call "%GBC%\SAMCexeject.bat"
"%GBC%\fart.exe" "SimCoupe.cfg" "[ROMPTH]\\" --remove
"%GBC%\fart.exe" "SimCoupe.cfg"  "%%~a"
)
exit /b
:addto
SET /A ADDTO+=1