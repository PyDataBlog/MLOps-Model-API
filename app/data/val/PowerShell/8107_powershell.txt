$ErrorActionPreference = 'Stop';

$packageName= 'Cryptomator'
$toolsDir   = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$url        = 'https://bintray.com/cryptomator/cryptomator/download_file?file_path=Cryptomator-1.2.3-x86.exe'
$url64      = 'https://bintray.com/cryptomator/cryptomator/download_file?file_path=Cryptomator-1.2.3-x64.exe'

$packageArgs = @{
  packageName   = $packageName
  unzipLocation = $toolsDir
  fileType      = 'exe'
  url           = $url
  url64bit      = $url64

  softwareName  = 'Cryptomator*'

  checksum      = '9732168A48506353F0A9FB1FB01F5E70A5FA113EB47A05F04DB05F4C8B6FFBBF'
  checksumType  = 'sha256'
  checksum64    = '2A375317A4850C42447E62BEE623ED57F0187BE029FAC510AA36BBAECBDB9B66'
  checksumType64= 'sha256'

  silentArgs   = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
  validExitCodes= @(0)
}

Install-ChocolateyPackage @packageArgs
