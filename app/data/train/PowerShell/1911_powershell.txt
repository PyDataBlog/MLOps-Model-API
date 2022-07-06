Write-Output 'Configuring SQL TCP port...'
	
If (!(Get-module SQLPS)) {
	Push-Location
	Import-Module SQLPS -DisableNameChecking
	Pop-Location
}

$HostName = $env:COMPUTERNAME
$smo = 'Microsoft.SqlServer.Management.Smo.'
$InstanceName = (New-Object ($smo + 'Server') $HostName | Select Name).Name	
$ServiceName = "SQL Server ($InstanceName)"
$wmi = New-Object ($smo + 'Wmi.ManagedComputer')
$uri = "ManagedComputer[@Name='" + $HostName + "']/ServerInstance[@Name='" + $InstanceName + "']/ServerProtocol[@Name='Tcp']"
$tcp = $wmi.GetSmoObject($uri)
$wmi.GetSmoObject($uri + "/IPAddress[@Name='IPAll']").IPAddressProperties[1].Value="1433"
$tcp.alter()
Restart-Service -f $ServiceName
