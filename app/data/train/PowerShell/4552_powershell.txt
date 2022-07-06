# AD Module Check
Write-Host "Checking if the Active Directory Module is installed"
if ((Get-Module -name ActiveDirectory -ErrorAction SilentlyContinue | foreach { $_.Name }) -ne "ActiveDirectory")
{
  Write-Host Active Directory Management has not been added to this session, adding it now...
  Import-Module ActiveDirectory
}
else
{
Write-Host Active Directory Module is ready for commands. -backgroundcolor green -foregroundcolor yellow
Start-Sleep -s 1
}

$cred = Get-Credential $env:userdomain\$env:username

$Session = New-PSSession -ConfigurationName Microsoft.Exchange -ConnectionUri http://(URL)/PowerShell/ -Authentication Kerbeross -Credential $cred
Import-PSSession $Session

$timestamp = Get-Date -Format 'MM/dd/yyyy - h:mm:ss tt'

$dnsroot = '@' + (Get-ADDomain).dnsroot

# Groups not pulled from the template account
# $AdditionalGroups = ('','','','','','')

$filePath = Split-Path -Parent $MyInvocation.MyCommand.Definition
$importCSV = $filePath + '\accounts_to_make.csv'

$CSVdata = Import-CSV $importCSV

foreach ($cell in $CSVdata)
{
      $TicketNumber = $cell.'Ticket Number'.trim()
      $template_sAMAccountName = $cell.'Template Account'.trim()
      $new_FirstName = $cell.'First Name'.trim()
      $new_LastName = $cell.'Last Name'.trim()
      $new_sAMAccountName = $cell.'User_Logon'.trim()
      if ($new_sAMAccountName.length -gt 20)
        {
          $new_sAMAccountName = $new_sAMAccountName.substring(0,20)
        }
      $new_DisplayName = $new_FirstName + ' ' + $new_LastName
      $new_Name = $new_FirstName + ' ' + $new_LastName
      $new_UserPrincipalName = $new_sAMAccountName + $dnsroot
      $new_primaryEmailAddress = $new_FirstName.replace(" ", "") + "." + $new_LastName.replace("'", "") + "@domain.com"
      $new_secondaryEmailAddress = $new_FirstName.replace(" ", "") + "." + $new_LastName.replace("'", "") + "@domain.net"
      $new_thirdEmailAddress = $new_FirstName.replace(" ", "") + "_" + $new_LastName.replace("'", "") + "@domain.net"
      $new_Password = 'Abc1234'
      $new_Office = $cell.'Office'.trim()
      $new_DriveLetter = 'U:'
      $new_HomeDir = $cell.'Home Drive Path'.trim() + $new_sAMAccountName
      $new_Manager = $cell.Manager.trim()
      $mgr = Get-ADUser -Identity $new_Manager
      $mgrOU = ($mgr.DistinguishedName -split ",", 2)[1]
      $new_Title = $cell.'Title / Position'.trim()
      $new_Department = $cell.Department.trim()
      $new_Description = 'AA/B//CCCCC/' + $new_Department + ' - ' + $timestamp
      $new_Notes = $cell.Notes.trim()
      $enable_afterCreation = $true
      $change_passAtLogon = $true
      $accountExpires = ([DateTime]($cell.'Account Expiration')).AddDays(1)

      $cloneADInstance = Get-Aduser $template_sAMAccountName -Properties city,company,country,postalCode,ScriptPath,State,StreetAddress,MemberOf

      $params = @{'SamAccountName' = $new_sAMAccountName;
                  'Instance' = $cloneADInstance;
                  'DisplayName' = $new_DisplayName;
                  'GivenName' = $new_FirstName;
                  'Surname' = $new_LastName;
                  'ChangePasswordAtLogon' = $change_passAtLogon;
                  'Description' = $new_Description;
                  'Title' = $new_Title;
                  'Department' = $new_Department;
                  'Office' = $new_Office;
                  'Manager' = $new_Manager;
                  'Enabled' = $enable_afterCreation;
                  'UserPrincipalName' = $new_UserPrincipalName;
                  'AccountPassword' = (ConvertTo-SecureString -AsPlainText $new_Password -Force);
                  'OtherAttributes' = @{'info'=$TicketNumber + ' - ' + $timestamp + ' - ' + $new_Notes};
                  }

      # Create the new user account using template account
      Get-ADUser -Filter {SamAccountName -eq $template_sAMAccountName} -Properties city,company,country,postalCode,ScriptPath,State,StreetAddress | New-ADUser -Name $new_Name @params

      # Mirror template Security Groups
      $cloneADInstance.MemberOf |
      ForEach-Object {
			  $_ | Add-ADGroupMember -Members $new_sAMAccountName
		  }
      
      # Additional Groups to add
      # $AdditionalGroups | Add-ADGroupMember -Members $new_sAMAccountName

      # Move the new user to Manager's OU
      Get-ADUser $new_sAMAccountName | Move-ADObject -TargetPath $mgrOU

      New-Item $new_HomeDir -type Directory -Force
        $ACL = Get-Acl "$new_HomeDir"
        $ACL.Access | ForEach { [Void]$ACL.RemoveAccessRule($_) }
        $ACL.AddAccessRule((New-Object System.Security.AccessControl.FileSystemAccessRule("DOMAIN\$new_sAMAccountName","FullControl", "ContainerInherit, ObjectInherit", "None", "Allow")))
        $ACL.AddAccessRule((New-Object System.Security.AccessControl.FileSystemAccessRule("DOMAIN\Domain Admins","FullControl", "ContainerInherit, ObjectInherit", "None", "Allow")))
        $ACL.AddAccessRule((New-Object System.Security.AccessControl.FileSystemAccessRule("DOMAIN\Administrator","FullControl", "ContainerInherit, ObjectInherit", "None", "Allow")))
        $ACL.AddAccessRule((New-Object System.Security.AccessControl.FileSystemAccessRule("BUILTIN\Administrators","FullControl", "ContainerInherit, ObjectInherit", "None", "Allow")))
        Set-Acl "$new_HomeDir" $ACL

      Set-ADUser -Identity $new_sAMAccountName -HomeDrive "$new_DriveLetter" -HomeDirectory "$new_HomeDir"

      Set-ADAccountExpiration $new_sAMAccountName -DateTime $accountExpires -Server (Get-ADDomain).PDCEmulator

      Enable-Mailbox -Identity $new_sAMAccountName

      Set-Mailbox -Identity $new_sAMAccountName -EmailAddresses $new_primaryEmailAddress,$new_secondaryEmailAddress,$new_thirdEmailAddress -Alias $new_sAMAccountName
}
