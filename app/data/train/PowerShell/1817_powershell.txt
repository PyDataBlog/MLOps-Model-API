<#
.SYNOPSIS
Switches the Haxe version used by the current shell instance.

.PARAMETER Version 
Accepted values: 2 or 3.
#>

Param(
    [ValidateRange(2, 3)]
    [int]$Version)

$haxe2Path = 'C:\Motion-Twin\Haxe'
$neko2Path = 'C:\Motion-Twin\neko'
$haxe3Path = 'C:\HaxeToolkit\haxe'
$neko3Path = 'C:\HaxeToolkit\neko'

if ($Version -eq 2)
{
    $env:Path = $env:Path.Replace($haxe3Path, $haxe2Path)
    $env:Path = $env:Path.Replace($neko3Path, $neko2Path)
    $env:HAXEPATH = $haxe2Path
}
elseif ($Version -eq 3)
{
    $env:Path = $env:Path.Replace($haxe2Path, $haxe3Path)
    $env:Path = $env:Path.Replace($neko2Path, $neko3Path)
    $env:HAXEPATH = $haxe3Path
}