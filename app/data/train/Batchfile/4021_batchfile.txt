@echo off
set "UPX_Path=C:\Users\..."
set "exePath=C:\...\dist\yourProgram.exe"
set "setuppyPath=C:\Users\...setup.py" rem setup.py in same dir as main python program

"%setuppyPath%" py2exe
pause
"%UPX_Path%" "%exePath%"
pause