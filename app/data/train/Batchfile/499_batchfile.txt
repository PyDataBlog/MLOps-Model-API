for /F "delims=. tokens=1,2" %%i in (esa.ver) do set EsaVersion=%%i.%%j

set DesignFormsDir=DesignForms_%EsaVersion%

ver | find "5.1." > nul
if %ERRORLEVEL% == 0 goto WinXP_CZ
 
:Win7
echo Running Windows 7 Script
set PublicFolderDir=c:\Users\Public\Documents\%DesignFormsDir%
set UserFolderDir=%userprofile%\Documents\%DesignFormsDir%
goto run
 
:WinXP_CZ
echo Running Windows XP Script
set PublicFolderDir=c:\Documents and settings\All users\Dokumenty\%DesignFormsDir%
set UserFolderDir=%userprofile%\Dokumenty\%DesignFormsDir%
goto run
 
:run
rem del "%PublicFolderDir%\*.*" /F /Q /S
xcopy _PublicDocuments\*.* "%PublicFolderDir%\*.*" /ys
rem xcopy Forms\Codes\*.clc "%PublicFolderDir%\Forms\Codes\*.*" /ys
xcopy _UserDocuments\*.* "%UserFolderDir%\*.*" /ys
