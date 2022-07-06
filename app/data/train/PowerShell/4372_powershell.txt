Import-Module "C:\ProgramData\chocolatey\chocolateyinstall\helpers\chocolateyInstaller.psm1"
$url="https://raw2.github.com/allensanborn/1001monkeysBoxstarter/master/webpicmdfix/WebpiCmd.bat"
$file="C:\ProgramData\chocolatey\bin\WebpiCmd.bat"
Get-ChocolateyWebFile "webpicmdfix" "$file" "$url"
#Fix stupid whitespace issue...
#http://stackoverflow.com/questions/724083/unix-newlines-to-windows-newlines-on-windows
Get-ChildItem $file | ForEach-Object{
    if (Get-Content $_.FullName -Delimiter "`0" | Select-String "[^`r]`n")
    {
    	$content = Get-Content $_.FullName
    	$content | Set-Content $_.FullName
    }
}