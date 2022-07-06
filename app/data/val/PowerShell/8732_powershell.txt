# ==============================================================================================
# 
# NAME: XPathTools.psm1
# 
# AUTHOR: Ben Baird
# DATE  : 8/2/2011
# 
# COMMENT:
# Contains a set of cmdlets for managing the Path environment variable.
# ==============================================================================================

Set-StrictMode -Version 2

#  .ExternalHelp XPathTools.psm1-Help.xml
function Get-Path
{
	[CmdletBinding()]
    param (
    	[switch]
    	$Itemized,
        [switch]
        $UserEnvironment
    )

	if ($UserEnvironment.IsPresent)
	{
		$sEnv = $(Get-ItemProperty -Path 'HKCU:\Environment' -ErrorAction SilentlyContinue)
	}
	else
	{
		$sEnv = $(Get-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Session Manager\Environment' -ErrorAction SilentlyContinue)
	}
	
	if (Get-Member -InputObject $sEnv -Name "Path")
	{
		if ($Itemized.IsPresent)
		{
			return $sEnv.Path.Split(';')
		}
		else
		{
			return $sEnv.Path
		}
	}
	else
	{
		Write-Error "There is no Path variable to retrieve."
	}
}

#  .ExternalHelp XPathTools.psm1-Help.xml
function Set-Path
{
	[CmdletBinding()]
    param (
        [parameter(Mandatory=$true,ValueFromPipeline=$true)]
        [string]$Path,
        [switch]
        $UserEnvironment
    )

	if ($UserEnvironment.IsPresent)
	{
		Set-ItemProperty -Path 'HKCU:\Environment' -Name "Path" -Value $Path -Force
		return $true
	}
	else
	{
		Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Session Manager\Environment' -Name "Path" -Value $Path -Force
		return $true
	}
	return $false
}

#  .ExternalHelp XPathTools.psm1-Help.xml
function Add-Path
{
	[CmdletBinding()]
    param (
        [parameter(Mandatory=$true,ValueFromPipeline=$true)]
        [string]$Path,
        [switch]
        $UserEnvironment
    )

	#
	# A folder path with and a folder path without a trailing
	# backslash obviously resolve to the same location, but
	# the -contains operator won't catch this, so we'll have
	# to account for both possibilities.
	#
	if ($Path.EndsWith('\'))
	{
		$withoutTrail = $Path.Substring(0, $Path.Length - 1)
		$withTrail = $Path
	}
	else
	{
		$withoutTrail = $Path
		$withTrail = $Path + '\'
	}

	#
	# Is this path already present in the environment variable?
	#
    if ($UserEnvironment.IsPresent)
    {
	    if (($(Get-Path -Itemized -UserEnvironment) -contains $withoutTrail) -or ($(Get-Path -Itemized -UserEnvironment) -contains $withTrail))
	    {
		    Write-Error "The specified path is already present. The Path variable was not modified."
		    return
	    }
    }
    else
    {
	    if (($(Get-Path -Itemized) -contains $withoutTrail) -or ($(Get-Path -Itemized) -contains $withTrail))
	    {
		    Write-Error "The specified path is already present. The Path variable was not modified."
		    return
	    }
    }

	#
	# Retrieve current Path.
	#
	$origPath = $null
	if ($UserEnvironment.IsPresent)
	{
		$origPath = $(Get-Path -UserEnvironment -ErrorAction SilentlyContinue)
	}
	else
	{
		$origPath = $(Get-Path -ErrorAction SilentlyContinue)
	}

	#
	# Append the new folder to the Path.
	#
	if ($origPath -eq $null)
	{
		$newPath = $withoutTrail
	}
	elseif ($origPath.EndsWith(';'))
	{
		$newPath = $origPath + $withoutTrail
	}
	else
	{
		$newPath = $origPath + ";" + $withoutTrail
	}
	
	#
	# Save.
	#
	if ($UserEnvironment.IsPresent)
	{
		Set-Path -Path $newPath -UserEnvironment
	}
	else
	{
		Set-Path -Path $newPath
	}
}

#  .ExternalHelp XPathTools.psm1-Help.xml
function Remove-Path
{
	[CmdletBinding()]
    param (
        [parameter(Mandatory=$true,ValueFromPipeline=$true)]
        [AllowEmptyString()]
        [string]$Path,
        [switch]
        $UserEnvironment
    )

	#
	# A folder path with and a folder path without a trailing
	# backslash obviously resolve to the same location, but
	# the -contains operator won't catch this, so we'll have
	# to account for both possibilities.
	#
	if ($Path.EndsWith('\'))
	{
		$withoutTrail = $Path.Substring(0, $Path.Length - 1)
		$withTrail = $Path
	}
	else
	{
		$withoutTrail = $Path
		$withTrail = $Path + '\'
	}

	if ($UserEnvironment.IsPresent)
	{
		[System.Collections.ArrayList]$pathlist = $(Get-Path -Itemized -UserEnvironment -ErrorAction SilentlyContinue)
	}
	else
	{
		[System.Collections.ArrayList]$pathlist = $(Get-Path -Itemized -ErrorAction SilentlyContinue)
	}

	#
	# Remove the item if it's present, otherwise error.
	#
	foreach ($item in $pathlist)
	{
		if (($item -eq $withoutTrail) -or ($item -eq $withTrail))
		{
			$pathlist.Remove($item)
			if ($UserEnvironment.IsPresent)
			{
				Set-Path -Path $([string]::Join(';', $pathlist.ToArray())) -UserEnvironment
			}
			else
			{
				Set-Path -Path $([string]::Join(';', $pathlist.ToArray()))
			}
			return
		}
	}
	Write-Error "The specified path was not present. The Path variable was not modified."
	return $false
}

