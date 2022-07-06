@echo off

REM Next command inputs the greeting
set location=bob
echo Hello %location%! This a sample batch file.

for /l %%x in (1, 1, 100) do (
   echo %%x
   copy %%x.txt z:\whatever\etc
)
pause

dir c:\windows
start /d "C:\Program Files\Microsoft Word" WINWORD.EXE

set yy=%date:~6,2%
set mm=%date:~3,2%
set dd=%date:~0,2%
if "%date:~6,1%"==" " set yy=0%yy:~1,1%
if "%date:~3,1%"==" " set mm=0%mm:~1,1%
if "%date:~0,1%"==" " set dd=0%dd:~1,1%
SET sys_date=20%yy%%mm%%dd%
