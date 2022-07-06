Install-ChocolateyZipPackage 'protoc' `
 'https://github.com/google/protobuf/releases/download/v2.6.1/protoc-2.6.1-win32.zip' `
 "$(Split-Path -parent $MyInvocation.MyCommand.Definition)" `
 -checksum 'b057f86ef83835010bb227eb2d82de04' -checksumType 'md5'