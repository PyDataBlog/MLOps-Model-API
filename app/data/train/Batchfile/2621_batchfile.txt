@echo off
rem /**
rem  * Copyright &copy; 2012-2016 <a href="https://github.com/thinkgem/jeesite">JeeSite</a> All rights reserved.
rem  *
rem  * Author: ThinkGem@163.com
rem  */
echo.
echo [ÐÅÏ¢] Éú³ÉEclipse¹¤³ÌÎÄ¼þ¡£
echo.
pause
echo.

cd %~dp0
cd..

call mvn deploy

cd bin
pause