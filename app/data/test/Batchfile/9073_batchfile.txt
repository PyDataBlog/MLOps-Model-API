rem @echo off
SETLOCAL EnableDelayedExpansion
for /f "tokens=1,2 delims=:,{} " %%a in (info.json) do (
    echo %%~a = %%~b
    set "%%~a=%%~b"
)
set "r_name=%name%_%version%"
echo "%r_name%"
IF EXIST ".\release\%r_name%" ( rmdir /S /Q ".\release\%r_name%" )
robocopy . ".\release\%r_name%" *.* /E /xf release.bat /xf *.xcf /xd release /xd .git
7z a -tzip ".\release\%r_name%.zip" ".\release\%r_name%"
ENDLOCAL
pause
