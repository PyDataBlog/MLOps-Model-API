for /d %%g IN (*) DO (
copy /Y "%GBC%\net\%LNCH%\%AMSTR%\WinAPE.ini" "%%~g\WinAPE.ini"
  set CSTM=%%~g
  pushd "%%~g"
CALL "%GBC%\Amstrinject.bat"
  call :injext
popd
)
  exit /b
:injext
call "%GBC%\CreateGamSubDir.bat"
for /f "tokens=* delims= " %%a in ('dir /b/a-d "%AMSTRDSK%"') do (
"%GBC%\fart.exe" "*.bat" .[EXT] %%~xa
)

:skp