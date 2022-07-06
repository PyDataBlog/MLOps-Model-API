@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\upbeat\bin\upbeat" %*
) ELSE (
  node  "%~dp0\..\upbeat\bin\upbeat" %*
)