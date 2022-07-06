@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"c:\Ruby21-x64\bin\ruby.exe" "c:/repos/refridged/.gem/2.1/ruby/2.1.0/bin/erubis" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"c:\Ruby21-x64\bin\ruby.exe" "%~dpn0" %*
