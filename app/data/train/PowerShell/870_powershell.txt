# halt immediately on any errors which occur in this module
$ErrorActionPreference = 'Stop'

function Invoke(

    [string]
    [Parameter(
        Position=0,
        Mandatory=$true,
        ValueFromPipeline=$true,
        ValueFromPipelineByPropertyName=$true)]
    $ParametersJson

){
    $Parameters = @{} 
    ($ParametersJson|ConvertFrom-Json).PSObject.Properties | %{$Parameters[$_.Name]=$_.Value}
    [PSCustomObject]$Parameters | New-AppeaseTaskTemplatePackage
}