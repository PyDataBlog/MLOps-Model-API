@echo off

:network
ipconfig /renew
ipconfig /flushdns

echo Done...
pause >nul