##################################################
#  WRITTEN BY NOOPZEN  - 2010
##################################################
param(
  $computer = $(throw "Please specify a computer name.")
  )


################
#FUNCTIONS
################
function ping-ip {  
    param( $ip )  
    trap {$false; continue} 
    $Trimout = 1000  
    $object = New-Object system.Net.NetworkInformation.Ping 
    (($object.Send($ip, $Trimout)).Status -eq 'Success')
    }


$ping = ping-ip $computer
if ($ping) {
	$model = Get-WmiObject -computername $computer -query "Select * from Win32_ComputerSystem" 
	if($model -ne $null){
	$tmp = $model | out-string
	$tmp = $tmp.TrimEnd()
	$out = "$tmp" 
	$out	
	}
	else{
	"ERROR WMI COULDNT CONNECT TO $computer ERROR"
	}
	
	#"waiting...."
	#read-host

	$cpu = Get-WmiObject -computername $computer -query "Select * from Win32_Processor" 
	
	if($cpu -ne $null){
	$tmp = $cpu[1].Name | out-string
	#$tmp
	#"waiting...."
	#read-host
	$tmp = $tmp.TrimEnd()
        #$tmp
	$out = "Processor`t: " + "$tmp"
	$out
 	$tmp = $cpu[1].CurrentClockSpeed | out-string
	$tmp = $tmp.TrimEnd()
	$out = "ProcessorSpeed`t: " + "$tmp"
	$out
 	$tmp = $cpu[1].DataWidth | out-string
	$tmp = $tmp.TrimEnd()
	$out = "ProcessorType`t: " + "$tmp"
	$out
	}
	else{
	"ERROR WMI COULDNT CONNECT TO $computer ERROR"
	}
}

else {
"Cannot ping $computer"
}
