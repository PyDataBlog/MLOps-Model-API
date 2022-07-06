#    Copyright 2015 42A Consulting
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
param([switch]$Force, [switch]$NoCommit)

$here = (Split-Path -Parent $MyInvocation.MyCommand.Path)

function Main()
{
	$existingVersion = [version]"0.0"
	$existingJarFile = Get-ChildItem $here -Filter "selenium-server-standalone-*.jar"
	if ($existingJarFile) {
		Write-Host "Attempting to update (replace) $existingJarFile"

		$m = [regex]::matches($existingJarFile, "-([0-9\.]+)\.jar")
		if ($m.Success) {
			$existingVersion = [version]$m.Groups[1].Value
		}
	} else {
		Write-Host "No existing .jar file found."
	}

	$webClient = New-Object System.Net.WebClient

	$downloadPageUri = "http://www.seleniumhq.org/download/"
	Write-Host "Trying to extract version and download URI from $downloadPageUri"

	$downloadPage = $webClient.DownloadString($downloadPageUri)
	$matches = [regex]::matches($page, 'Download version <a href="([^"]+)">([0-9\.]+)</a>', "IgnoreCase")
	if (-not $matches.Success) {
		throw "Could not find download information from page"
	}

	$version = $matches.Groups[2].Value
	Write-Host "Found version $version"

	if (([version]$version) -le $existingVersion -and (-not $Force)) {
		Write-Host "Remote version $version is less than or equal to local version $existingVersion"
		Write-Host "Cancelling update"
		return
	}

	$downloadUri = $matches.Groups[1].Value
	$downloadFile = ("selenium-server-standalone-{0}.jar" -f $version)

	Write-Host "GET $downloadUri -> $downloadFile"
	$webClient.DownloadFile($downloadUri, (Join-Path $here $downloadFile))
	Write-Host "Download complete"

	Write-Host "Updating composer.json"
	((Get-Content "$here\composer.json") -replace "selenium-server-standalone-[0-9\.]+\.jar", $downloadFile) | Out-File "$here\composer.json" -Encoding ASCII

	if (-not $NoCommit) {
		Write-Host "Creating git commit"
		
		if ($existingJarFile) {
			$commitMessage = "Update to selenium version $version"
			if ($existingJarFile -ne $downloadFile) {
				Exec { git rm $existingJarFile }
			}
		} else {
			$commitMessage = "Add selenium version $version"
		}

		Exec { git add $downloadFile }
		Exec { git add "composer.json" }
		Exec { git commit -m  $commitMessage }
		Exec { git tag -a "v$version" -m $commitMessage }
	}

	Write-Host ""
	Write-Host "All done"
}

function Exec([scriptblock]$cmd, [string]$errorMessage = "Error executing command: " + $cmd) { 
  & $cmd 
  if ($LastExitCode -ne 0) {
    throw $errorMessage 
  } 
}

Main
