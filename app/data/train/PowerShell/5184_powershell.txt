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
	if($model){
	$tmp = $model.model | out-string
	$tmp = $tmp.TrimEnd()
	$out = "$computer,$tmp" 
	$out	
	}
	else{
	"ERROR WMI COULDNT CONNECT TO $computer ERROR"
	}
}

else {
"Cannot ping $computer"
}
