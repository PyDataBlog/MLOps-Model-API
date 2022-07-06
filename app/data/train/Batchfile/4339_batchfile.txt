pushd "%GBC%\net\%LNCH%\%ZXSP%"
"%GBC%\fart.exe" "Default.spincfg" [EMUPTH] "%GBE%\%EMUZ%\%ZXSPIN%"
if "%GGOFF%"=="1" "%GBC%\fart.exe" "spin.ini" [SPINDIR] "%GBE%\%EMUZ%\%ZXSPIN%"
if "%GGOFF%"=="1" "%GBC%\fart.exe" "Default.spincfg" [SPINDir] "%GBE%\%EMUZ%\%ZXSPIN%"
if "%GGOFF%"=="1" copy /y "spin.ini" "%GBE%\%EMUZ%\%ZXSPIN%"
if "%GGOFF%"=="1" copy /y "Default.spincfg" "%GBE%\%EMUZ%\%ZXSPIN%"
popd

for /d %%g IN (*) DO (
  if "%RECONF%"=="1" copy /Y "%GBC%\net\%LNCH%\%ZXSP%\spin.ini" "%%~g"
  if "%RECONF%"=="1" copy /Y "%GBC%\net\%LNCH%\%ZXSP%\Default.spincfg" "%%~g"
  set CSTM=%%~g
  pushd "%%~g"
  call :injext
CALL "%GBC%\Zxspinject.bat"
popd
  )
exit /b
:injext
  for /f "tokens=* delims= " %%a in ('dir /b/a-d-h') do (
if /I "%%~xa"==".z80" "%GBC%\fart.exe" "*.bat" [EXT] z80
if /I "%%~xa"==".tzx" "%GBC%\fart.exe" "*.bat" [EXT] tzx
if /I "%%~xa"==".sna" "%GBC%\fart.exe" "*.bat" [EXT] sna
if /I "%%~xa"==".szx" "%GBC%\fart.exe" "*.bat" [EXT] szx
if /I "%%~xa"==".tap" "%GBC%\fart.exe" "*.bat" [EXT] tap
if /I "%%~xa"==".blk" "%GBC%\fart.exe" "*.bat" [EXT] blk
if /I "%%~xa"==".csw" "%GBC%\fart.exe" "*.bat" [EXT] csw
if /I "%%~xa"==".dsk" "%GBC%\fart.exe" "*.bat" [EXT] dsk
if /I "%%~xa"==".scr" "%GBC%\fart.exe" "*.bat" [EXT] scr
if /I "%%~xa"==".wav" "%GBC%\fart.exe" "*.bat" [EXT] wav
if /I "%%~xa"==".rom" "%GBC%\fart.exe" "*.bat" [EXT] rom
if /I "%%~xa"==".hdf" "%GBC%\fart.exe" "*.bat" [EXT] hdf
if /I "%%~xa"==".zip" "%GBC%\fart.exe" "*.bat" [EXT] zip
)

:skp