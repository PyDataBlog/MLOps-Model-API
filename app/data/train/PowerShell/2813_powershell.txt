Login-AzureRMAccount

$spParams = @{
    DisplayName = exampleapp 
    Password    = "{provide-password}"
}

$sp = New-AzureRmADServicePrincipal @spParams

$SleepParams = @{
    Seconds = 20
}

Start-Sleep @SleepParams

$AzureRoleAssignmentParams = @{
    RoleDefinitionName   = Contributor 
    ServicePrincipalName = $sp.ApplicationId
}

New-AzureRmRoleAssignment @AzureRoleAssignmentParams

$secpasswdParams = @{
    String      = "mmy8V6nOcOyGX0B" 
    AsPlainText = $true 
    Force       = $true
}

$secpasswd = ConvertTo-SecureString @secpasswdParams

$mycredsParams = @{
    TypeName     = System.Management.Automation.PSCredential 
    ArgumentList = ("768835e9-73f7-4a04-a5c4-b68c43b11477", $secpasswd)
}

$mycreds = New-Object @mycredsParams

$AzureRMAccountParams = @{
    Credential       = $mycreds 
    ServicePrincipal = $true 
    TenantId         = 72f988bf-86f1-41af-91ab-2d7cd011db47
}

Login-AzureRmAccount @AzureRMAccountParams