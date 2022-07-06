$user = Get-AzureADUser -UserPrincipalName "attila.ersek@scaleddomains.com"
$resourceGroup = Get-AzureResourceGroup -Name "ABK01"

New-AzureRoleAssignment -ObjectId $user.id.guid -RoleDefinitionName Contributor -Scope $resourceGroup.ResourceId

Get-AzureRoleAssignment -ObjectId $user.id.Guid -ResourceGroupName "ABK01" -RoleDefinitionName Contributor
Get-AzureRoleDefinition -Name Contributor

Remove-AzureRoleAssignment -ObjectId $user.id.guid -RoleDefinitionName Contributor -Scope $resourceGroup.ResourceId