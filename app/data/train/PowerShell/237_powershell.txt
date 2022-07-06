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
	$osver = Get-WmiObject -computername $computer -query "Select * From Win32_OperatingSystem" 
	if($osver){
	$tmp = $osver.CSName | out-string
	$tmp = $tmp.TrimEnd()
	$out += "$tmp`t" 
	$tmp = $osver.Caption    | out-string
	$tmp = $tmp.TrimEnd()
	$out += "$tmp`t"                                
	$tmp = $osver.CSDVersion  | out-string
	$tmp = $tmp.TrimEnd()
	$out += "$tmp`t"                               
	$tmp = $osver.OSArchitecture    | out-string  
	$tmp = $tmp.TrimEnd()                      
	$out += "$tmp`t" 
	$out	
	}
	else{
	"ERORR ERROR ERROR WMI COULDNT CONNECT TO $computer ERROR ERROR"
	}
}

else {
"Cannot ping $computer"
}
