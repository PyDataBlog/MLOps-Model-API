<#
.Synopsis
   Get all the running Virtual Machines from a host.

.DESCRIPTION
   Enumerate all the Virtualmachines from a Hyper-V host
   
   Requirements : 
   Windows Server 2012 R2 HYPER-V
   Windows Server 2012 R2

.PARAMETER HyperV
    This can be a single servername or an array of servers.
    This is mandatory parameter, so if this parameter is not provided
    the values will be asked.
.EXAMPLE
   .\Enum-vm.ps1 -HyperV <server1>,<Server2>,......

.EXAMPLE
   .\Enum-VM.ps1

        cmdlet Enum-VM.ps1 at command pipeline position 1
        Supply values for the following parameters:
        HyperV[0]: <Server1>
        HyperV[1]: <Server2>
        HyperV[2]:
#>
    [CmdletBinding()]
    Param
    (
        # Param1 help description
        [Parameter(Mandatory=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [Array]$HyperV
     )

Get-VM -ComputerName $HyperV | Select Computername,
	Name,
	State,
        @{l="Gen";e={$_.Generation}},
        CreationTime,
        @{l="Uptime";e={$_.uptime}},
        @{l="CPUs";e={$_.ProcessorCount}},
        @{l="MemoryMB";e={($_.memoryassigned/1MB)}},
        @{l="MemoryMin";e={$_.MemoryMinimum/1MB}},
        @{l="MemoryMax";e={$_.MemoryMaximum/1MB}},
        @{l="MacAddress";e={$_.networkadapters.macaddress}},
        @{l="IPAddress";e={$_.networkadapters.ipaddresses}},
        @{l="Status";e={$_.Status}},
        @{l="HDDs";e={$_.harddrives.count}},
        VMId | Out-gridview #FT -AutoSize