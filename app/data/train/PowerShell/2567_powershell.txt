$packageName = ".Net 3.5"

Write-Host "Installing $packageName" -ForegroundColor Green
Import-Module ServerManager
Add-WindowsFeature "NET-Framework-Features"
Write-Host "Finished installing $packageName" -ForegroundColor Green