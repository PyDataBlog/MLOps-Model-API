@echo off
CLS
%header%
echo.
if not exist "Programme\HackMii Installer" mkdir "Programme\HackMii Installer"
echo 		Downloade den HackMii Installer...
start /min/wait Support\wget -c -l1 -r -nd --retr-symlinks -t10 -T30 --random-wait --reject "*.html" --reject "%2A" --reject "get.php@file=hackmii_installer_v1.0*" "http://bootmii.org/download/"
move get.php* hackmii-installer_v1.2.zip >NUL
start /min/wait Support\7za e -aoa hackmii-installer_v1.2.zip -o"Programme\HackMii Installer" *.elf -r
del hackmii*
:endedesmoduls