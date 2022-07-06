@echo off
CALL npm install typescript
IF %ERRORLEVEL% NEQ 0 (
	ECHO Unable to install typescript
	GOTO Error
)

IF (%1%=="") GOTO SetTsc
cd %1%

:SetTsc
IF EXIST ".\node_modules\.bin\tsc.cmd" (
    SET tsc=CALL ".\node_modules\.bin\tsc.cmd"
    GOTO Build
)
IF EXIST "%ProgramFiles(x86)%\Microsoft Sdks\Typescript\0.9\tsc.exe" (
    SET tsc="%ProgramFiles(x86)%\Microsoft Sdks\Typescript\0.9\tsc.exe"
    GOTO Build
)
IF EXIST "%ProgramFiles%\Microsoft Sdks\Typescript\0.9\tsc.exe" (
    SET tsc="%ProgramFiles%\Microsoft Sdks\Typescript\0.9\tsc.exe"  
    GOTO Build
GOTO
ECHO TypeScript compiler not found
EXIT 999

:Build
ECHO Cleaning folder
del %DEPLOYMENT_TARGET%\*.* /S /Q
ECHO Building TypeScript: pblander.ts (using %tsc%)
%tsc% src\game\pblander.ts
xcopy %DEPLOYMENT_SOURCE%\src %DEPLOYMENT_TARGET% /S /Y

:Error
EXIT %ERRORLEVEL%