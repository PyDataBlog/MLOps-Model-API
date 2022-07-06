:UNSORT
del /q "%GBC%\currentlist.ini"
del /q "%GBC%\output_(currentlist).txt"
start "" "%GBC%\Wbusy.exe" "Indexing" "Indexing your library" /Marquee
echo.=>"%GBC%\currentlist.ini"
echo.ALL>>"%GBC%\currentlist.ini"
if not exist "%GBC%\conselect.ini" goto :INIT
CALL :NOTEXIST
if "%ISTNM%"=="" start "" "%GBC%\Wbusy.exe" "Indexing" "Complete" /Stop /timeout:1
if "%ISTNM%"=="" exit /b

:INIT
if not exist "%GBC%\conselect.ini" call :CONCAT
SET /A UNNUM=0
for /f "tokens=* delims= " %%a in ('dir /b/ad-h "%GBG%\%GAM%\%CONS%"') do (
SET CURLIST=%%~a
call :REMOVE
)

start "" "%GBC%\Wbusy.exe" "Indexing" "Complete" /Stop /timeout:1
call :STRIPINI
call :TEST
for %%R in ("%GBC%\currentlist.ini") do if %%~zR leq 8 exit /b
goto :UNEMPTY

:NOTEXIST
set ISTNM=
for /f "tokens=* delims= " %%a in ('dir /b /ad-h "%GBG%\%GAM%\%CONS%"') do (
SET ALIST=%%~a
call :INTESTS
)
exit /b

:CONCAT
for /f "Tokens=* delims= " %%a in ('type "%GBC%\conselect.set"') do echo.%%~a>>"%GBC%\conselect.ini"
exit /b

:INTESTS
for /f "tokens=* delims= " %%a in ('type "%GBC%\conselect.ini"') do if "%%~a"=="%ALIST%" set ISTNM=1
exit /b

:REMOVE
SET HERE=
for /f "tokens=* delims= " %%a in ('type "%GBC%\conselect.ini"') do if /i "%CURLIST%"=="%%~a" SET HERE=1
REM if "%HERE%"=="" move /y "%GBG%\%GAM%\%CONS%\%CURLIST%" "%GBG%\%ROMJ%"
if "%HERE%"=="" echo.%CURLIST%>>"%GBC%\currentlist.ini"
if "%HERE%"=="" SET /A UNNUM+=1
if "%HERE%"=="" start "" "%GBC%\Wbusy.exe" "Indexing" "%UNNUM% systems found" /Marquee
exit /b

:TEST
set ALLSET=1
for %%R in ("%GBC%\currentlist.ini") do if %%~zR leq 8 SET ALLSET=0
exit /b

:UNEMPTY
if "%UNNUM%"=="0" exit /b
start "" "%GBC%\Wbusy.exe" "Indexing" "Complete" /Stop /timeout:1
"%GBC%\wbox.exe" "UNKNOWN-SYSTEMS" "^####################################################^%UNNUM% unknown or improperly named systems have been found in your library.^####################################################^^        You may choose to:^^[IMPORT] unidentified consoles as custom consoles^^[SORT] unidentified consoles to your RJ-Sort-Folder^^[DETECT] unidentified consoles using fuzzy matching logic.^^[MATCH] an unidentified console to a supported console.^^[IGNORE] improperly named consoles and continue jacketizing.^" "IMPORT;SORT;DETECT;RENAME;IGNORE;HELP" /AL /DB=3
if %ERRORLEVEL%==1 goto :ADD
if %ERRORLEVEL%==2 goto :MOVE
if %ERRORLEVEL%==3 goto :MATCH
if %ERRORLEVEL%==4 goto :RENAME
if %ERRORLEVEL%==5 goto :EOF
if %ERRORLEVEL%==6 goto :HELP
goto :EOF

:RENAME
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "ALL" "=="
"%GBC%\wselect.exe" "%GBC%\currentlist.ini" "Select to rename" "set UNKNO=$item" > "%GBC%\unknown.cmd"
if %errorlevel% == 0 goto :UNEMPTY
call "%GBC%\unknown.cmd"
if /i "%UNKNO%"=="" goto :RENAME
if /i "%UNKNO%"=="=" goto :RENAME
if /i "%UNKNO%"=="==" goto :RENAME
if /i "%UNKNO%"=="ALL" goto :RENAME
"%GBC%\wselect.exe" "%GBC%\conselect.set" "%UNKNO% matches" "set KNOWN=$item" > "%GBC%\known.cmd"
if %errorlevel% == 0 goto :UNEMPTY
call "%GBC%\known.cmd"
if exist "%GBG%\%GAM%\%CONS%\%KNOWN%" (robocopy "%GBG%\%GAM%\%CONS%\%UNKNO%" "%GBG%\%GAM%\%CONS%\%KNOWN%" /R:2 /W:2 /NP /E /IS /MOVE	 && "%GBC%\fart.exe" "%GBC%\currentlist.ini" "%UNKNO%" --remove) ELSE (rename "%GBG%\%GAM%\%CONS%\%UNKNO%" "%KNOWN%" && "%GBC%\fart.exe" "%GBC%\currentlist.ini" "%UNKNO%" --remove)
SET /A UNNUM+=-1
goto :UNEMPTY

:HELP
"%GBC%\docs\sort.html"
"%GBC%\wbox.exe" "opening" "Help is opening" "OK" /TM=3
goto :UNEMPTY

:MOVE
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "==" "ALL"
"%GBC%\wselect.exe" "%GBC%\currentlist.ini" "System integration" "set UNKNO=$item" > "%GBC%\unknown.cmd"
if %errorlevel% == 0 exit /b
call "%GBC%\unknown.cmd"
if "%UNKNO%"=="ALL" goto :TORJF
if "%UNKNO%"=="=" goto :MOVE
if "%UNKNO%"=="" exit /b
move /y "%GBG%\%GAM%\%CONS%\%UNKNO%" "%GBG%\%ROMJ%" && set /A UNNUM+=-1
goto :UNEMPTY

:ADD
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "==" "ALL"
"%GBC%\wselect.exe" "%GBC%\currentlist.ini" "System integration" "set UNKNO=$item" > "%GBC%\unknown.cmd"
if %errorlevel% == 0 exit /b
call "%GBC%\unknown.cmd"
if "%UNKNO%"=="ALL" goto :ADDALL
if "%UNKNO%"=="=" goto :ADD
if "%UNKNO%"=="" exit /b
echo.%UNKNO%>>"%GBC%\conselect.ini"
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "%UNKNO%" --remove
CALL :STRIPINI
call :TEST
if "%ALLSET%"=="0" exit /b
set /A UNNUM+=-1
goto :UNEMPTY

:MATCH
start "" "%GBC%\Wbusy.exe" "Renaming" "RomJacket is attempting to identify and catalogue your unidentified systems." /Marquee
CALL "%GBC%\presort.bat"
start "" "%GBC%\Wbusy.exe" "Renaming" "Complete" /Stop /timeout:1
CALL :STRIPINI
call :TEST
if "%ALLSET%"=="0" goto :UNEMPTY
goto :UNEMPTY

:TORJF
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "ALL" --remove
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "==" --remove
"%GBC%\fart.exe" "%GBC%\currentlist.ini" "=" --remove
for /f "tokens=* delims= " %%a in ('type "%GBC%\currentlist.ini"') do move /y "%%~a" "%GBG%\%ROMJ%"
exit /b

:ADDALL
for /f "tokens=* delims= " %%a in ('type "%GBC%\currentlist.ini"') do echo.%%~a>>"%GBC%\conselect.ini"
set /a UNNUM=0
exit /b

:STRIPINI
for %%A in ("%GBC%\currentlist.ini") do call :Strip %%A
exit /b

:Strip
For %%x in (%*) do set OutF=output_(%%~nx).txt
del %OutF%
for /F "tokens=* delims=" %%B in (%*) do echo %%B >> %OutF%
Goto :EOF