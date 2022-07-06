@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\mean-cli\bin\mean-status" %*
) ELSE (
  node  "%~dp0\..\mean-cli\bin\mean-status" %*
)