%GAMDIR%
cd "%GBG%\%ROMJ%\%WSTMP%"

:setchk
for /f "tokens=* delims= " %%t in ('dir /b /a-d "%GBG%\%ROMJ%\%WSTMP%"') do (
SET CHKFILE=
SET NMCHK=%%~nt
set EXTCHK=%%~xt
call :CHKSUM
call :dirchk
)
goto :same

:CHKSUM
if /i "%EXTCHK%" EQU ".2d" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gg" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".ws" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".vb" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".md" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gb" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".nrg" set CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".elf" set CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".img" set CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".iso" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".cdi" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".cdz" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".ccd" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".bin" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gen" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".smd" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".hdf" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".d64" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".n64" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".z64" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".jag" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".mds" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".lnk" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".sna" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".tzx" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".z80" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".wsc" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".cue" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".ngp" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".npk" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".32x" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".nds" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gcm" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".nd5" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".sfc" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".smc" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".pce" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".vec" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".pbp" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".pgm" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".td0" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".cpb" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".2dd" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".msa" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".st" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".vboy" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".com" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".obx" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".ssd" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".uef" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".cas" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".dcm" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".xfd" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".pro" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".bas" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".atz" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".atx" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".atr" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".unf" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".nes" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gbc" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".a78" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".a26" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".a52" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".gba" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".int" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".tap" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".rom" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".swf" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".neo" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".ngc" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".dsk" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".rp2" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".dol" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".col" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".wad" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".wbfs" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".32x" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".sg" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".adf" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".sms" SET CHKFILE=%NMCHK%
if /i "%EXTCHK%" EQU ".7z" CALL :extract
)
exit /b

:extract
for /f "tokens=* delims= " %%a in ('dir /b /a-d "*.7z"') do "%SEVENZIP%" x -y "%GBG%\%ROMJ%\%WSTMP%\%%~a" -o"%GBG%\%ROMJ%\%WSTMP%"
exit /b

:dirchk
if "%CHKFILE%" EQU "" goto :same
if "%CHKFILE%" EQU "%WSTMP%" goto :same

:rename
mkdir "%CHKFILE%"
move /Y "%CHKFILE%*.*" "%CHKFILE%"
:same