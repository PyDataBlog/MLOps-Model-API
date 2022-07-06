for /f "tokens=* delims= " %%a in ('dir /b/a-d "%GBE%\%EMUZ%\%NULLDC%\Data\*"') do (
set BIOSN=%%~na
set BIOSF=%%~nxa
set BIOSA=%%~a
call :BIOSPUT
)
goto :INTP

:BIOSPUT
copy /y "%GBE%\%EMUZ%\%NULLDC%\Data\%BIOSF%" "%GBC%\net\%EXECT%\%CSTCONS%\%BIOSF%"
echo.copy /y "%BIOSF%.bak" "Data\%BIOSF%">>"%GBC%\bioscpy.ini"
exit /b

:INTP
pushd "%GBC%\net\%EXECT%\%SEGDC%"
for /d %%g IN ("%CSTM%") DO (
set CSTM=%%~g
set /A ADDTO=0
call :injext
if "%ADDTO%" GEQ "2" set FNDMORE=1
CALL "%GBC%\SEGDCexeject.bat"
)
popd

exit /b
:injext
pushd "%GBC%\net\%EXECT%\%SEGDC%"
  for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%CSTM%"') do (
set DCROM=%%~a
if /I "%%~xa"==".cdi" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] cdi
if /I "%%~xa"==".mdf" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] mdf
if /I "%%~xa"==".mds" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] mds
if /I "%%~xa"==".nrg" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] nrg
if /I "%%~xa"==".gdi" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] gdi
if /I "%%~xa"==".chd" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] chd
if /I "%%~xa"==".iso" call :addto && "%GBC%\fart.exe" "%GBC%\net\%EXECT%\%CSTCONS%\*.bat" [EXT] iso
if /I "%%~xa"==".cdi" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".mdf" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".mds" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".nrg" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".gdi" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".chd" CALL "%GBC%\Segdcexeject.bat"
if /I "%%~xa"==".iso" CALL "%GBC%\Segdcexeject.bat"
)
exit /b
:addto
SET /A ADDTO+=1