REM  QBFC Project Options Begin
REM  HasVersionInfo: Yes
REM  Companyname: by leian1306
REM  Productname: Borrar cola de impresion
REM  Filedescription: Borra la cola de impresion
REM  Copyrights: Mine!
REM  Trademarks: 
REM  Originalname: 
REM  Comments: 
REM  Productversion:  1. 0. 0. 0
REM  Fileversion:  1. 0. 0. 0
REM  Internalname: 
REM  Appicon:
REM  QBFC Project Options End

@echo off& MODE CON: COLS=60 LINES=8
color 0f
title Borrar cola de impresion :: by leian1306

echo.
echo   Presione una tecla para comenzar con el proceso...
pause>nul
echo.
net stop spooler
ping -n 3 localhost>nul
cls

echo.
echo   Borrando cola de impresion...
del /f /q %windir%\system32\spool\PRINTERS\*.*
ping -n 3 localhost>nul
cls

echo.
net start spooler
ping -n 3 localhost>nul
cls

echo.
echo   LISTO! Ahora prueba a imprimir.
echo   Si no llegara a funcionar,reinicia la impresora
echo   y/o la computadora.
echo.
echo   Presiona una tecla para salir...
pause>nul

