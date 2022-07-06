# There's usually much more than this in my profile!
$SCRIPTPATH = "C:\Users\Speed\Documents\WindowsPowerShell"
$PROGRAMPATH = "D:\Program Files (x86)"

$VIMPATH    = $PROGRAMPATH + "\vim\vim74\vim.exe"
$VIMRUNTIME = $PROGRAMPATH + "\vim\vim74"

Set-Alias vi   $VIMPATH
Set-Alias vim  $VIMPATH
Set-Alias c	   		"cls"

Function pjs	{ cd D:\workspace\js }
Function pjava  { cd D:\workspace\java }
Function pcpp 	{ cd 'D:\workspace\C&CC' }

$Host.UI.RawUI.BackgroundColor = 'black'

# for editing your PowerShell profile
Function Edit-Profile
{
    #vim $profile
	vim "C:\Users\Speed\Documents\WindowsPowerShell\profile.ps1"
}

# for editing your Vim settings
Function Edit-Vimrc
{
    vim $home\_vimrc
}

#for gihub
if ($env:github_shell -eq $null) {

  Write-Verbose "Running GitHub\shell.ps1"

  Push-Location (Split-Path -Path $MyInvocation.MyCommand.Definition -Parent)

  $env:github_posh_git = Resolve-Path "$env:LocalAppData\GitHub\PoshGit_3874a02de8ce2b7d4908a8c0cb302294358b972c"
  $env:github_git = Resolve-Path "$env:LocalAppData\GitHub\PortableGit_c2ba306e536fdf878271f7fe636a147ff37326ad"
  $env:PLINK_PROTOCOL = "ssh"
  $env:TERM = "msys"
  $env:HOME = Resolve-Path (join-path ([environment]::getfolderpath("mydocuments")) "..\")
  $env:TMP = $env:TEMP = [system.io.path]::gettemppath()
  if ($env:EDITOR -eq $null) {
    $env:EDITOR = "GitPad"
  }

  # Setup PATH
  $pGitPath = $env:github_git
  $appPath = Resolve-Path "$env:LocalAppData\Apps\2.0\7XV8L8WZ.4RB\BRWCR8MC.4RN\gith..tion_317444273a93ac29_0002.0006_159c473f7cb71f94"
  $msBuildPath = "$env:SystemRoot\Microsoft.NET\Framework\v4.0.30319"

  $env:Path = "$env:Path;$pGitPath\cmd;$pGitPath\bin;$pGitPath\mingw\bin;$appPath;$msbuildPath"

  if (!$SkipSSHSetup) {
    & (Join-Path $appPath GitHub.exe) --set-up-ssh
  }

  Pop-Location

} else { Write-Verbose "GitHub shell environment already setup" }

#for test_83
Function test_83
{
  #
}

#for psreadline
Install-Module psreadline

#for ip_address

Function ipadd () {

	$ipaddress = [System.Net.DNS]::GetHostByName($null)

	foreach($ip in $ipaddress.AddressList)
	{
		if ($ip.AddressFamily -eq 'InterNetwork')
		{
			$ip.IPAddressToString
		}
	}
}

$chocolatey_bin_root="D:\Program Files"


