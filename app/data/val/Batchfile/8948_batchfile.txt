@echo off
@DEM YOUNEEDTOHAVEPOWERSHELL
echo $storageDir = $pwd >C:/temp/runfile.ps1
echo $webclient = New-Object System.Net.WebClient >C:/temp/runfile.ps1
echo $url = "%1" >C:/temp/runfile.ps1
echo $file = "$storageDir\%2" >C:/temp/runfile.ps1
echo $webclient.DownloadFile($url,$file) >C:/temp/runfile.ps1
PowerShell -NoProfile -ExecutionPolicy Bypass -Command "& {Start-Process PowerShell -ArgumentList '-NoProfile -ExecutionPolicy Bypass -File ""C:/temp/runfile.ps1""' -Verb RunAs}"
del C:/temp/runfile.ps1 /F /Q
cls
