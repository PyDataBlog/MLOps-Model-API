##################################################
#  WRITTEN BY NOOPZEN  - 2010
##################################################

param( $target)
$block = { 

import-module 'webAdministration'
Get-WebApplication | %{
	$pool = $_.applicationpool; 
	$state = (Get-WebAppPoolState $pool).value; 
	if($state -eq "stopped"){write-host -f red "$pool $state"}
	else{write-host -f green "$pool $state"}
	}

}
write-host -f cyan "Checking app pools on  $target"
Invoke-Command -computername $target -ScriptBlock $block
