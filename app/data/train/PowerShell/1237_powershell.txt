#
# Unmount-ImageFile.ps1
#

# This function will unmount a wim or vhd file using DISM.
Function Unmount-ImageFile
{
	[CmdletBinding()]
	Param ( 

		[Parameter(Mandatory=$true,Position=0)]
		[string]$MountDir
	)

# Verify the mount directory exists
	Begin
	{
		if (Test-Path $MountDir)
		{
			Write-Verbose "$MountDir exists."
		}
		else
		{
			Write-Warning "$MountDir does NOT exist and cannot be unmounted"
			BREAK;
		}
	}

# Unmount the image file and commit changes
	Process
	{
		write-verbose "Attemping to unmount image file"
		try
			{
				Invoke-Expression "dism /Unmount-Image /MountDir:$MountDir /Commit"
			}
			catch [System.Exception]
			{
				Write-Warning "Unable to unmount $MountDir"
				BREAK;
			}
	}
	
	# Verify the MountDir is gone
	End
	{
		if (Test-Path $MountDir)
		{
			Write-Warning "$MountDir was not successfully unmounted"
		}
		else
		{
			write-verbose "$MountDir has been successfully unmounted."
		}
	}
}
