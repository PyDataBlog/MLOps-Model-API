@ECHO off
SETLOCAL

REM This variable should not be defined after execution
SET FooBar=abc

REM This should produce an error
dir DirectoryThatHopefullyDoesNotExist

REM Skip to the end when the last command doesn't succeed
IF %ERRORLEVEL% NEQ 0 GOTO END

REM This command should never be executed
dir

:END
ENDLOCAL & EXIT /B %ERRORLEVEL%