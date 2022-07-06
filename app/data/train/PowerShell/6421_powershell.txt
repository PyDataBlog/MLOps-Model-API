
$ErrorActionPreference = 'Stop';

$packageName= 'fbexport'
$toolsDir   = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$url        = 'http://www.firebirdfaq.org/files/fbexport-1.90.tar.gz'

$packageArgs = @{
  packageName   = $packageName
  unzipLocation = $toolsDir
  fileType      = 'exe'
  url           = $url

  softwareName  = 'FBExport*'

  checksum      = 'c4f768f07225b68bf8cde11021a7b02a'
  checksumType  = 'md5'
  validExitCodes= @(0)
}

Install-ChocolateyZipPackage @packageArgs
$File = Get-ChildItem -File -Path $env:ChocolateyInstall\lib\$packageName\tools\ -Filter *.tar
Get-ChocolateyUnzip -fileFullPath $File.FullName -destination $env:ChocolateyInstall\lib\$packageName\tools\










    








