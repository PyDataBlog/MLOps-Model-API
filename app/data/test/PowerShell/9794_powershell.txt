function RunSendEmail
{
	$FromAddress = "no-reply-Exch-Services@domain.com"
	#$ToAddress = "alerts@domain.com,c@domain.com"
	$ToAddress = "c@domain.com"
	$MessageSubject = "Exchange Service Failure(s)"
	$MessageBody = $NewBody
	$SendingServer = "casarray.domain.local"

	###Create the mail message and add the statistics text file as an attachment
	$SMTPMessage = New-Object System.Net.Mail.MailMessage $FromAddress,$ToAddress,$MessageSubject,$MessageBody
	$SMTPMessage.IsBodyHtml = $true
	###Send the message
	$SMTPClient = New-Object System.Net.Mail.SMTPClient $SendingServer
	$SMTPClient.Send($SMTPMessage)
}

#Filters the types of services you want to check.
Get-WmiObject Win32_Service | Where-Object {$_.Name -like '*Exchange*' -and $_.StartMode -eq 'Auto' -and $_.State -eq 'Running'} | Select-Object Name | export-csv C:\temp\services.csv -NoTypeInformation

#Logic. If the file is not empty then there are failures.
$NewBody = "The following list are services found that are listed as to be Automatically running.`f"
$NewBody = $NewBody + "and are currently stopped / not running.`f`f"
$NewBody = $NewBody + "<u><b>Failed Services:</b></u>`f"
$test = Import-CSV C:\temp\services.csv
if ($test -ne $NULL)
{
	Foreach ($line in $test)
	{
		$NewLine = $line.Name
		$NewLine = $NewLine + "`f"
		$NewBody = $NewBody + $NewLine
		
	}
	RunSendEmail
}