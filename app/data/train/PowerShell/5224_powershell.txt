$packageName = 'MakeMKV'
$installerType = 'EXE' 
$url = 'http://www.makemkv.com/download/Setup_MakeMKV_v1.9.10.exe' # download url
$url64 = 'http://www.makemkv.com/download/Setup_MakeMKV_v1.9.10.exe' # 64bit URL uses the same as $url
$silentArgs = '/S' # NSIS installar uses /S 
$validExitCodes = @(0) 
$is64bit = Get-ProcessorBits 64
if ($is64bit) {
	$unpath = "${Env:ProgramFiles(x86)}\$packageName\uninst.exe"
} else {
	$unpath = "${Env:ProgramFiles}\$packageName\uninst.exe"
}

if (Test-Path $unpath ) {
	try {
		# Prompt to uninstall a previous version, as unnstalling silently is not supported, the uninstaller runs async with UI.
		Start-ChocolateyProcessAsAdmin "$silentArgs" "$unpath" -validExitCodes $validExitCodes
	}
	catch {
		# Silently swallow errors on uninstall.
	}
	Write-ChocolateyFailure $packageName "As MakeMKV does not currently offer a silent uninstaller, please manually uninstall the program and run the uninstalle command again to remove the Chocolatey package."
}
else {
	# The uninstaller actually does nothing under the current restrictions with the Nulloft uninstaller for MakeMKV.
	# Uninstall-ChocolateyPackage "$packageName" "$installerType" "$silentArgs" "$unpath" -validExitCodes $validExitCodes
}