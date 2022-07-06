@echo off
:: Special launcher to start notepad++ with 2nd to 9th arguments.
:: It's only needed when using Image File Execution to replacing Notepad.exe.
:: http://stackoverflow.com/questions/2984846/
::
:: Activate with:
::
::  reg add "HKLM\Software\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\notepad.exe" /v ^
::  "Debugger" /t REG_SZ /d "c:\bin\nppLauncher.bat" /f
:: 
:: Remove with:
::
::  reg delete "HKLM\Software\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\notepad.exe"
::

:: This used to have fancier IF EXIST/ELSE and used %ProgamFiles% variables, 
:: but it was devilishly difficult to get bug free.
if exist "C:\Program Files\Notepad++\notepad++.exe" start "" "C:\Program Files\Notepad++\notepad++.exe" %2 %3 %4 %5 %6 %7 %8 %9
if exist "C:\Program Files (x86)\Notepad++\notepad++.exe" start "" "C:\Program Files (x86)\Notepad++\notepad++.exe" %2 %3 %4 %5 %6 %7 %8 %9
