# To be PS 2.0 compatible, dont use Invoke-WebRequest

$packageName = "DotNet3_5.exe"
$downloadUrl = "https://download.microsoft.com/download/7/0/3/703455ee-a747-4cc8-bd3e-98a615c3aedb/dotNetFx35setup.exe"
$targetFile = "C:\tmp\$packageName"

# Download installer
Write-Host "Downloading $packageName" -ForegroundColor Green
(New-Object System.Net.WebClient).DownloadFile($downloadUrl, $targetFile)

Write-Host "Installing $packageName" -ForegroundColor Green
Start-Process -FilePath $targetFile -ArgumentList "/q","/norestart" -Wait
Write-Host "Finished installing $packageName" -ForegroundColor Green