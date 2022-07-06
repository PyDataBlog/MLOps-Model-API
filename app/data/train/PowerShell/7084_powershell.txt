param (
   [string]$ResourceGroupName = $(if ($env:ResourceGroupName -eq $null) {$( Read-Host "Input Azure resource group name" )} else {$env:ResourceGroupName}),
   [string]$VmName = $(if ($env:VmName -eq $null) {$( Read-Host "Input Azure VM Name" )} else {$env:VmName})
)

Stop-AzureRmVM -ResourceGroupName $ResourceGroupName -Name $VmName
