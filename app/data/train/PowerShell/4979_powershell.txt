Write-Host "Configure Agent"

$AgentName = "NotSet";
$PoolName = "NotSet";
$WorkFolder = "NotSet";
$Url = "https://" + $Env:VSTS_ACCOUNT + ".visualstudio.com";

if([string]::IsNullOrEmpty($Env:VSTS_AGENT))
{
  $AgentName = $Env:COMPUTERNAME;
}
else
{
  $AgentName = $Env:VSTS_AGENT;
}

if([string]::IsNullOrEmpty($Env:VSTS_POOL))
{
  $PoolName = "Default";
}
else 
{  
  $PoolName = $Env:VSTS_POOL; 
}

if([string]::IsNullOrEmpty($Env:VSTS_WORK))
{
  $WorkFolder = "_work";
}
else 
{
  $WorkFolder = $Env:VSTS_WORK;
}

Write-Host "AgentName: $AgentName"
Write-Host "Url: $Url"
Write-Host "Poolname: $PoolName"
Write-Host "WorkFolder: $WorkFolder"

./config.cmd --unattended --agent $AgentName --url $Url --auth PAT --token $Env:VSTS_TOKEN --pool $PoolName --work $WorkFolder --replace

./run.cmd