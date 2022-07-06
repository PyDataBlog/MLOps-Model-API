# ------------------------------------------------------------------------------
# Create Virtual Machine Wizard Script
# ------------------------------------------------------------------------------
# Script generated on Saturday, April 29, 2017 10:20:41 AM by Virtual Machine Manager
# 
# For additional help on cmdlet usage, type get-help <cmdlet name>
# ------------------------------------------------------------------------------


New-SCVirtualScsiAdapter -VMMServer localhost -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da -AdapterID 7 -ShareVirtualScsiAdapter $false -ScsiControllerType DefaultTypeNoType 


New-SCVirtualDVDDrive -VMMServer localhost -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da -Bus 1 -LUN 0 

$VMNetwork = Get-SCVMNetwork -VMMServer localhost -Name "External Network" -ID "42c0f4fd-d02d-4846-a0ff-3d47aee229eb"

New-SCVirtualNetworkAdapter -VMMServer localhost -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da -MACAddress "00:00:00:00:00:00" -MACAddressType Static -VLanEnabled $false -Synthetic -EnableVMNetworkOptimization $false -EnableMACAddressSpoofing $false -EnableGuestIPNetworkVirtualizationUpdates $false -IPv4AddressType Dynamic -IPv6AddressType Dynamic -VMNetwork $VMNetwork 


Set-SCVirtualCOMPort -NoAttach -VMMServer localhost -GuestPort 1 -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da 


Set-SCVirtualCOMPort -NoAttach -VMMServer localhost -GuestPort 2 -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da 


Set-SCVirtualFloppyDrive -RunAsynchronously -VMMServer localhost -NoMedia -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da 

$CPUType = Get-SCCPUType -VMMServer localhost | where {$_.Name -eq "3.60 GHz Xeon (2 MB L2 cache)"}


New-SCHardwareProfile -VMMServer localhost -CPUType $CPUType -Name "Profile3ddf90a5-3443-4739-832e-9a19ffd0a94f" -Description "Profile used to create a VM/Template" -CPUCount 1 -MemoryMB 512 -DynamicMemoryEnabled $false -MemoryWeight 5000 -VirtualVideoAdapterEnabled $false -CPUExpectedUtilizationPercent 20 -DiskIops 0 -CPUMaximumPercent 100 -CPUReserve 0 -NumaIsolationRequired $false -NetworkUtilizationMbps 0 -CPURelativeWeight 100 -HighlyAvailable $false -DRProtectionRequired $false -NumLock $false -BootOrder "CD", "IdeHardDrive", "PxeBoot", "Floppy" -CPULimitFunctionality $false -CPULimitForMigration $false -Generation 1 -JobGroup b464e3d4-4d1a-4bd1-828f-a45b18df98da 




New-SCVirtualDiskDrive -VMMServer localhost -IDE -Bus 0 -LUN 0 -JobGroup 46acd607-e357-43f0-9c33-d1e7fe04c2e7 -VirtualHardDiskSizeMB 40960 -CreateDiffDisk $false -Dynamic -Filename "Win2012Lab9_disk_1" -VolumeType BootAndSystem 

$HardwareProfile = Get-SCHardwareProfile -VMMServer localhost | where {$_.Name -eq "Profile3ddf90a5-3443-4739-832e-9a19ffd0a94f"}

New-SCVMTemplate -Name "Temporary Template6eb5fe58-1589-44b2-90d3-ef2aad6b321d" -Generation 1 -HardwareProfile $HardwareProfile -JobGroup 46acd607-e357-43f0-9c33-d1e7fe04c2e7 -NoCustomization 



$template = Get-SCVMTemplate -All | where { $_.Name -eq "Temporary Template6eb5fe58-1589-44b2-90d3-ef2aad6b321d" }
$virtualMachineConfiguration = New-SCVMConfiguration -VMTemplate $template -Name "Win2012Lab9"
Write-Output $virtualMachineConfiguration
$vmHost = Get-SCVMHost -ID "fb1a6055-7273-4183-a404-8e567679a3c2"
Set-SCVMConfiguration -VMConfiguration $virtualMachineConfiguration -VMHost $vmHost
Update-SCVMConfiguration -VMConfiguration $virtualMachineConfiguration
Set-SCVMConfiguration -VMConfiguration $virtualMachineConfiguration -VMLocation "E:\20409" -PinVMLocation $true

$AllNICConfigurations = Get-SCVirtualNetworkAdapterConfiguration -VMConfiguration $virtualMachineConfiguration



Update-SCVMConfiguration -VMConfiguration $virtualMachineConfiguration
$operatingSystem = Get-SCOperatingSystem | where { $_.Name -eq "Windows Server 2012 R2 Standard" }
New-SCVirtualMachine -Name "Win2012Lab9" -VMConfiguration $virtualMachineConfiguration -Description "Lab 9 exercise" -BlockDynamicOptimization $false -JobGroup "46acd607-e357-43f0-9c33-d1e7fe04c2e7" -ReturnImmediately -StartAction "NeverAutoTurnOnVM" -StopAction "SaveVM" -OperatingSystem $operatingSystem
