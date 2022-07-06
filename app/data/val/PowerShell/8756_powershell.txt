$packageName = 'PSWindowsUpdate' # arbitrary name for the package, used in messages
$url = 'http://gallery.technet.microsoft.com/scriptcenter/2d191bcd-3308-4edd-9de2-88dff796b0bc/file/41459/43/PSWindowsUpdate.zip' # download url
$url64 = 'http://gallery.technet.microsoft.com/scriptcenter/2d191bcd-3308-4edd-9de2-88dff796b0bc/file/41459/43/PSWindowsUpdate.zip' # 64bit URL 

try { 
  $installDir = Join-Path $PSHome "Modules"

  Install-ChocolateyZipPackage "$packageName" "$url" "$installDir" "$url64"
     
  Write-ChocolateySuccess "$packageName"
} catch {
  Write-ChocolateyFailure "$packageName" "$($_.Exception.Message)"
  throw 
}