
%GBD%
cd "%GBC%"
for /f "tokens=* delims= " %%a in ('dir /b/ad-h "%GBG%\%GAM%\%HYPS%"') do (
SET HYPSYS=%%~a
pushd "%GBG%\%GAM%\%HYPS%\%%~a"
CALL :HSGAMADD
CALL :HSGAMFIN
CALL :UPDATE
popd
)
GOTO :EOF

:UPDATE
if "%CLSTR%"=="~0,1" set CLSTR=
if "%CLSTR%"=="" exit /b
if "%CLCHNG%" NEQ "%CLSTR%" start "" "%GBC%\Wbusy.exe" "BuildingXMLS" "Building %HYPSYS% XMLs"  /marquee
SET CLCHNG=%CLSTR%
exit /b


:HSGAMADD
set CLSTR=%HYPSYS%
type "%GBC%\net\%HYPS%\HSGamBegin.prc">"%GBC%\net\%HYPS%\%HYPSYS%.xml"
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%GBG%\%GAM%\%HYPS%\%HYPSYS%"') do (
copy /Y "%GBC%\net\%HYPS%\HSGamAdd.prc" "%GBC%\net\%HYPS%\%HYPSYS%.prc"
SET HYPGAM=%%~na
pushd "%GBC%\net\%HYPS%"
"%GBC%\fart.exe" "%HYPSYS%.prc" [GAM] "%%~na"
popd
type "%GBC%\net\%HYPS%\%HYPSYS%.prc">>"%GBC%\net\%HYPS%\%HYPSYS%.xml"
copy /Y "%GBC%\net\%HYPS%\Setting.int" "%GBC%\net\%HYPS%\%HYPSYS%.inj"
)
exit /b

:HSGAMFIN
type "%GBC%\net\%HYPS%\HSGamEnd.prc">>"%GBC%\net\%HYPS%\%HYPSYS%.xml"
del /q "%GBC%\net\%HYPS%\%HYPSYS%.prc"
mkdir "%HPN%\Databases\%HYPSYS%"
for /f "tokens=* delims= " %%a in ('dir /b /a-d-h "%GBC%\net\%HYPS%\*.xml"') do copy /Y "%GBC%\net\%HYPS%\%%~a" "%HPN%\Databases\%%~na\%%~a"