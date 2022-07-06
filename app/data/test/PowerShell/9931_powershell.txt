<#
.DESCRIPTION
    Switch the physical directory of an IIS Website.
#>
param (
     $physicalPath = "S:\w3schools\Pairs"
)

$WebAdministrationModule = 'WebAdministration'

If ( ! (Get-module $WebAdministrationModule )) {
    Write-Host "Loading module $WebAdministrationModule"
    Import-Module $WebAdministrationModule
}

<#
    List all sites.
#>
Set-Location IIS:

Get-ChildItem .

$site = Get-ChildItem Sites | Where-Object { $_.Name -eq "Default Web Site" }

Set-ItemProperty 'IIS:\Sites\Default Web Site\' -Name physicalPath -Value $physicalPath
