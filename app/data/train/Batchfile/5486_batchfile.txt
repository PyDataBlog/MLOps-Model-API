REM Set the process name as process_name below
if [%1]==[] (
	exit
)
set process_name=%1
REM Set timout value in seconds
set timeout_val=10
if not [%2]==[] (
	set timeout_val=%2
)
	
@ECHO OFF
:start
tasklist /FI "IMAGENAME eq %process_name%" 2>NUL | find /I /N "%process_name%">NUL
if "%ERRORLEVEL%" NEQ "0" cmd /c start %process_name%
timeout %timeout_val%
goto start



