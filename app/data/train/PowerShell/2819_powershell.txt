function New-CredStore {
	
	<#
  .SYNOPSIS
  Creates a credstore to store PSCredentials
  .DESCRIPTION
  Creates a store of credentials associated with a "friendly-name"(credname), each credential in the store is then accessible by calling the storename and appending the credname
	
  Credentials are stored as PSCredential objects and the password are stored as System.Security.SecureString objects
  .EXAMPLE
  add-credstore domainuser
	the above command asks for credentials to be associated with the credname "domainuser" and stores under the default storename $credstore, to access the cerdentials simply use: $credstore.domainuser
  .EXAMPLE
  add-credstore domainuser,superuser -storename mydomain
	the above command asks for credentials to be stored with each of the input crednames and stores them under the credstore $mydomain, to access the credentials simply use: $mydomain.domainuser or $mydomain.superuser
  .PARAMETER crednames
  Friendly name to associate with stored credentials
  .PARAMETER storename
  Name of the variable created that stores the credentials
  #>
	
	[cmdletbinding()] 
	Param #defining input parameters
	(
		[parameter(ValueFromPipeline = $true, Mandatory = $true, Position = 0)]
		[String[]]
		$crednames,
		
		[parameter(ValueFromPipeline = $false)]
		[String]
		$storename="credstore"
	)

	begin #runs once before main function
	{
		New-Variable -Name $storename -Value (@{ }) -Scope global
		$tempvarname = (Get-Variable -Name $storename).name
		$tempvarvalue = (Get-Variable -Name $storename).value
		Write-Output "Adding the following to the credstore"
	}

	process #runs once for each item in the pipline
	{
		foreach ($credname in $crednames)
		{
			$cred = Get-Credential -Message "Please provide credentials"
			$tempvarvalue.add($credname, $cred)
			#Write-Output($cred.UserName + " has been associated with " + $credname + " and can be accessed with $" + $tempvar.name + "." + $credname)
		}
	}
	end #runs once after main function
	{
		$tempvarvalue = (Get-Variable -Name $storename).value
		$tempvarname = (Get-Variable -Name $storename).name
		$output = @{ }
		$tempvarvalue.keys | foreach { $output.Add("$" + $storename + "." + $_, $tempvarvalue.$_.username) }
		$output
		
	}
}


function Get-CredStore
{
	param
	(
		[parameter(ValueFromPipeline = $false, Position = 0)]
		[String]
		$storename = "credstore"
	)
	process
	{
		try
		{
			$tempvarvalue = (Get-Variable -Name $storename).value
			$tempvarname = (Get-Variable -Name $storename).name
			$output = @{ }
			$tempvarvalue.keys | foreach { $output.Add("$" + $storename + "." + $_, $tempvarvalue.$_.username) }
			$output
		}
		catch
		{
			Write-Error "No CredStore found with that name."
			Write-Error "The default Credstore to search is $credstore, if you specified another storename when creating the credstore please append that to the cmdlet"
			break
		}
		#finally
	}
	
}