$i=(Get-WmiObject -Class Win32_NetworkAdapterConfiguration -ComputerName . | where {$_.IPAddress.length -gt 1}).ipaddress[0]
switch -wildcard ($i)
    {
        "192.168.51.*" {"VS: $i"; (Get-WmiObject -Class Win32_Printer -ComputerName . -Filter "Name='VS Physicians Xerox WorkCentre 3550 Class Driver'").SetDefaultPrinter()}
        "192.168.52.*" {"WB: $i"; (Get-WmiObject -Class Win32_Printer -ComputerName . -Filter "Name='WB Physicians Xerox Phaser 3600 Class Driver'").SetDefaultPrinter()}
        "192.168.53.*" {"NB: $i"; (Get-WmiObject -Class Win32_Printer -ComputerName . -Filter "Name='NB Physicians Xerox Phaser 3600 Class Driver'").SetDefaultPrinter()}

        default {"No ip found $i"}
    }
