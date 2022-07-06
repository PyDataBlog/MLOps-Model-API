# set the path as the path for loading modules 
# (I don't concatenate the existing path here to avoid x-platform differences complications)
$env:PSMODULEPATH = $PSScriptRoot

Import-Module Bar

# Call the imported module
$res = BarHello
Write-Host $res
