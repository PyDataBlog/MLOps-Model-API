@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\mochiscript\bin\ms-compile" %*
) ELSE (
  node  "%~dp0\..\mochiscript\bin\ms-compile" %*
)