$ErrorActionPreference = 'Stop'

Install-ChocolateyZipPackage `
    -PackageName 'libxml2' `
    -Url 'http://xmlsoft.org/sources/win32/libxml2-2.7.8.win32.zip' `
    -UnzipLocation "$(Split-Path -parent $MyInvocation.MyCommand.Definition)" `
    -Checksum '4F21BFAEA68290053C58266F9E3182A662C9B5BB0743869CB116964E9EDAC55F8CC932D17038DFBB60678B385861FB0D1E7FAA4ED1859EA49C3095634E29F2CA' `
    -ChecksumType 'sha512'
    
Install-ChocolateyPath `
    -PathToInstall "$(Split-Path -parent $MyInvocation.MyCommand.Definition)\libxml2-2.7.8.win32\bin"
