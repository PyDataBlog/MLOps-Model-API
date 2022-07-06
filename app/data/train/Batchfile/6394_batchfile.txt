:XMLS
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h/s "%HPN%\Databases\*.orig"') do SET HSXMLDIR=%%~a
if "%HSXMLDIR%" NEQ "" goto :xmlbak
:xmlorig
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h/s "%HPN%\Databases\*.xml"') do copy /Y "%%~dpnxa" "%%~dpna.orig"
:xmlbak
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h/s "%HPN%\Databases\*.xml"') do copy /Y "%%~dpnxa" "%%~dpna.bak"

:INIS
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%HPN%\Settings\*.orig"') do SET HSINIDIR=%%~a
if "%HSINIDIR%" NEQ "" goto :inibak
:iniorig
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%HPN%\Settings\*.ini"') do copy /Y "%%~dpnxa" "%%~dpna.orig"
:inibak
for /f "tokens=* delims= " %%a in ('dir /b/a-d-h "%HPN%\Settings\*.ini"') do copy /Y "%%~dpnxa" "%%~dpna.bak"

rename "%HPN%\Settings\Settings.ini" "Settings.inj"
rename "%HPN%\Settings\Main Menu.ini" "Main Menu.inj"
del /q "%HPN%\Settings\*.ini"
rename "%HPN%\Settings\Settings.inj" "Settings.ini"
rename "%HPN%\Settings\Main Menu.inj" "Main Menu.ini"
