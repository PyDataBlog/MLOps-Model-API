set TOUHLNK=
for /d %%g IN ("%CSTM%") DO (
  set CSTM=%%~g
CALL "%GBC%\Touhuexeject.bat"
  pushd "%%~g"
  set /A ADDTO=0
call :injext
if "%ADDTO%" GEQ "2" set FNDMORE=1
popd 
 )
  exit /b
:injext
pushd "%GBC%\net\%EXECT%\%TOUHU%"
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%CSTM%"') do (
if /I "%%~xa"==".lnk" set TOUHLNK=%%~a
if /I "%%~xa"==".lnk" CALL "%GBC%\touhou2exeject.bat"
)
exit /b
:addto
SET /A ADDTO+=1