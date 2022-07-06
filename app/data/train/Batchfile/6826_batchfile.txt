echo.%userprofile%>"%GBC%\user.ini"
FOR /F "tokens=* delims= " %%a in ('type "%CD%\user.ini"') DO SET CURSR=%%~a
FOR /F "usebackq tokens=3 skip=1" %%i in (`REG QUERY "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders" /v Desktop`) DO (
SET DESKTOPDIR=%%i
)
FOR /F "usebackq delims=" %%i in (`ECHO %DESKTOPDIR%`) DO (
SET DESKTOPDIR=%%i
)
echo.%DESKTOPDIR%>"%GBC%\desk.ini"

pushd "%GBC%"
if exist "%GBG%\%GAM%\%CONS%" (goto :setupLink) ELSE (goto :linkedup)

:SHORTCUTS

:setupLink
copy /y "%GBC%\dnd.prt" "%GBC%\RJPrep.vbs"
copy /y "%GBC%\dnd.prt" "%GBC%\BiosSelect.vbs"
copy /y "%GBC%\dnd.prt" "%GBC%\START.vbs"
"%GBC%\fart.exe" "START.vbs" [GBC] "%GBC%"
"%GBC%\fart.exe" "BiosSelect.vbs" [GBC] "%GBC%"
"%GBC%\fart.exe" "RJprep.vbs" [GBC] "%GBC%"
"%GBC%\fart.exe" "RJprep.vbs" [BAT] "RJprep.bat"

"%GBC%\fart.exe" "START.vbs" [BAT] "START.bat"
"%GBC%\fart.exe" "BiosSelect.vbs" [BAT] "BiosSelect.bat"

"%GBC%\xxmklink.exe" "RJ-Sort-Folder.lnk" "%GBC%\RJPrep.vbs" "" "%GBC%" Rom-Jacket-Folder 7 "%GBC%\Jacket.ico"
"%GBC%\xxmklink.exe" "RJ-Configurator.lnk" "%GBC%\START.vbs" "" "%GBC%" Rom-Jacket 7 "%GBC%\RomJacket.ico"
"%GBC%\xxmklink.exe" "RJ-Bios-Folder.lnk" "%GBC%\BiosSelect.vbs" "" "%GBC%" BIOS-Folder 7 "%GBC%\Bios.ico"
FOR /F "tokens=* delims= " %%a in ('type "%GBC%\desk.ini"') DO SET USKDP=%%~a

move /Y "*.lnk" "%USKDP%"
copy /y mod.set rjf.vbs
for %%A in ("%GBC%\Jacket.ico") do set ICON=%%~A
SET ICON=%GBC%\Jacket.ico
SET NAME=Rom-Jacket-Folder.lnk

"%GBC%\fart.exe" rjf.vbs "[ICON]" "%ICON%"
"%GBC%\fart.exe" rjf.vbs "[NAME]" "%NAME%"
cmd /c cscript //B rjf.vbs

del /q rjf.vbs

copy /y mod.set rjf.vbs
for %%A in ("%GBC%\RomJacket.ico") do set ICON=%%~A
SET NAME=RJ-Configurator.lnk

"%GBC%\fart.exe" rjf.vbs "[ICON]" "%ICON%"
"%GBC%\fart.exe" rjf.vbs "[NAME]" "%NAME%"
cmd /c cscript //B rjf.vbs

del /q rjf.vbs

copy /y mod.set rjf.vbs

for %%A in ("%GBC%\Bios.ico") do set ICON=%%~A
SET NAME=Bios-Folder.lnk
"%GBC%\fart.exe" rjf.vbs "[ICON]" "%ICON%"
"%GBC%\fart.exe" rjf.vbs "[NAME]" "%NAME%"
cmd /c cscript //B rjf.vbs

:linkedup
"%GBC%\mkfav.bat"
popd