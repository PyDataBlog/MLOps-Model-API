@echo off
rem Courtesy of [gr1m](http://stackoverflow.com/users/3902635/gr1m).
rem Source: http://stackoverflow.com/a/25097964
title WinVim
color a
cls
echo WinVim 1.02
echo.
echo To save press CTRL+Z then press enter
echo.
echo Make sure to include extension in file name
set /p name=File Name:
copy con %name%
if exist %name% copy %name% + con
