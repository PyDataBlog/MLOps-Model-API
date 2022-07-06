@echo off

REM change up to the parent (structured) directory
cd ..

REM capture the full path of current directory
set cwd=%CD%

REM setup php path
set PATH=%PATH%;%cwd%\vendor\php

REM setup dynamic vars
set port=8097

REM wipe and old instance of dynamic files
type conf\template.httpd.conf|bin\repl.bat "80" "%port%" > conf\httpd.conf
REM extension_dir = "../vendor/php/ext"


REM change to Apache directory and run Apache
echo %cwd%
echo "Starting structuredfun web server on port %port%"
cd vendor\Apache24 && bin\httpd.exe -f ..\..\conf\httpd.conf

pause

