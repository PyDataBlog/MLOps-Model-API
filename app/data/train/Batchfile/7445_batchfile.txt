pushd "%GBC%\net\%EXECT%\%ARCD%"
"%GBC%\fart.exe" "Mame32.ini" [SOURCE] "[ROMPTH]\SOURCE"
popd
for /d %%g IN ("%CSTM%") DO (
  mkdir "%%~g\cfg"
  mkdir "%%~g\ini"
  mkdir "%%~g\ctrl"
  set CSTM=%%~g
  set /A ADDTO=0
call :injext
if "%ADDTO%" GEQ "2" set FNDMORE=1
CALL "%GBC%\Arcdexeject.bat"
  )
   exit /b
   
:injext
pushd "%GBC%\net\%EXECT%\%ARCD%"
  for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%CSTM%"') do (
if /I "%%~xa"==".zip"call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] zip
if /I "%%~xa"==".lnk"call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] lnk
popd
)
exit /b
:addto
SET /A ADDTO+=1