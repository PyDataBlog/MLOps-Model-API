net stop wuauserv
net stop cryptSvc
net stop bits
net stop msiserver
ren %WINDIR%\SoftwareDistribution\Download Download.bak
ren %WINDIR%\SoftwareDistribution\DataStore\DataStore.edb DataStore.bak
ren %SYSTEM%\catroot2 catroot2.bak
net start wuauserv
net start cryptSvc
net start bits
net start msiserver
pause