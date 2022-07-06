@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\mean-cli\bin\mean-preinstall" %*
) ELSE (
  node  "%~dp0\..\mean-cli\bin\mean-preinstall" %*
)