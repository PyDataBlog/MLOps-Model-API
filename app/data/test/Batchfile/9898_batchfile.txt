@echo off
echo Please type the name of the folder you want to make followed by Enter, then Ctrl-Z, and then Enter.
FOR /F "tokens=*" %%A IN ('TYPE CON') DO SET INPUT=%%A
mkdir %INPUT%
pause
echo Folder created
pause
