@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\fleet-panel\bin\start" %*
) ELSE (
  node  "%~dp0\..\fleet-panel\bin\start" %*
)