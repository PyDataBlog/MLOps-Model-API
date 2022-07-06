@echo off
rem processes them through without lint error :)
if exist log.txt del log.txt
for /f %%a IN (_list.txt) do (
d:\tools\mikero\makepbo -LN %%a >>log.txt
)

rem copy missions to <root>/missions/ dir
if exist *.pbo move *.pbo "D:\Wintools\Steamshit\SteamApps\common\Arma 3\Missions\"
pause
exit
