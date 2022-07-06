pushd "%GBC%\net\%EXECT%\%NGBA%"
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%GBE%\%EMUZ%\%VGBA%\GBA*.rom"') do set NGBABIOS=%%~a
"%GBC%\fart.exe" "%VGBA%m.ini" [EMUL] "[EMUPTH]"
"%GBC%\fart.exe" "%VGBA%m.ini" [NGBABIOS] "%NGBABIOS%"
copy /y "%GBE%\%EMUZ%\%VGBA%\%NGBABIOS%" "%GBC%\net\%EXECT%\%NGBA%"
popd
for /d %%g IN ("%CSTM%") DO (
set CSTM=%%~g
set /A ADDTO=0
call :injext
if "%ADDTO%" GEQ "2" set FNDMORE=1
CALL "%GBC%\Ngbaexeject.bat"
)
exit /b
:injext
pushd "%GBC%\net\%EXECT%\%NGBA%"
  for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%CSTM%"') do (
if /I "%%~xa"==".gba" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] gba
if /I "%%~xa"==".zip" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] zip
popd
)
exit /b
:addto
SET /A ADDTO+=1