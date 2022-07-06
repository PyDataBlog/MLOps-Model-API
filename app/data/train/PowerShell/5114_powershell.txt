$packageName = 'wtee' # arbitrary name for the package, used in messages

try
{ 
    # Resolve needed values.
    $tools = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
    $savePath = Join-Path $tools "wtee.exe"

    Get-ChocolateyWebFile $packageName $savePath 'https://wintee.googlecode.com/files/wtee.exe'

    Write-ChocolateySuccess $packageName
}
catch
{
    Write-ChocolateyFailure $packageName "$($_.Exception.Message)"
    throw 
}