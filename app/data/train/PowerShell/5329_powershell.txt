# Connect to Microsoft Online

Import-Module MSOnline

Connect-MsolService

# Retrieve the list of all users in Spain

$users = Get-MsolUser -All -Country Spain

# Create a new CSV file
$file = New-Item c:\Temp\users.csv -type file -force
Add-Content $file """Username"",""Name"",""Licenses"""
foreach ( $user in $users ) {
    Add-Content $file """$($user.UserPrincipalName)"",""$($user.DisplayName)"",""$($user.Licenses.AccountSkuId)"""
}
