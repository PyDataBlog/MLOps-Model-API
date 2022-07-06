$ErrorActionPreference = 'Stop'; # stop on all errors
$packageName= 'altap-salamander' # arbitrary name for the package, used in messages
$toolsDir   = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$url        = 'https://www.altap.cz/ftp/salamand/as308x86.exe' # download url
$url64      = 'https://www.altap.cz/ftp/salamand/as308x64.exe' # 64bit URL here or remove - if installer is both, use $url
$packageArgs = @{
  packageName   = $packageName
  unzipLocation = $toolsDir
  fileType      = 'EXE' #only one of these: exe, msi, msu
  url           = $url
  url64bit      = $url64
  silentArgs = '/x /s'
  validExitCodes = @(0)
  softwareName  = 'altap-salamander*' #part or all of the Display Name as you see it in Programs and Features. It should be enough to be unique
  checksum      = '59FB36FDBACCBA9ECC0C58832B05ACD646F98CB5EE423CD748391C2FDAF4063012D9141EAED83F2E2DED2AF24029AE46C3E90186418B05D483617966A8842919'
  checksumType  = 'sha512' #default is md5, can also be sha1
  checksum64    = '803B69FB4C3B1E3199A858BCD986D9A673F89493B30818FD4A221DCE2137A26521302C7CBEB26D66DF0946A4333D62AE38A8BA7C8976E94DDDC5A20BA9803C5D'
  checksumType64= 'sha512' #default is checksumType
}

Install-ChocolateyPackage @packageArgs
