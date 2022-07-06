# ==============================================================================
# 
# Joe Niland Copyright 2014 - Present - Released under the Apache 2.0 License
# Based on Chocolatey install script: https://chocolatey.org/install.ps1
# 
# Copyright 2007-2008 The Apache Software Foundation.
#  
# Licensed under the Apache License, Version 2.0 (the "License"); you may not use 
# this file except in compliance with the License. You may obtain a copy of the 
# License at 
#
#     http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software distributed 
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
# CONDITIONS OF ANY KIND, either express or implied. See the License for the 
# specific language governing permissions and limitations under the License.
# ==============================================================================

# variables
$url = "https://originalmind.io/r/boxstarter-latest"
$chocTempDir = Join-Path $env:TEMP "boxstarter"
$tempDir = Join-Path $chocTempDir "install"
if (![System.IO.Directory]::Exists($tempDir)) {[System.IO.Directory]::CreateDirectory($tempDir)}
$file = Join-Path $tempDir "boxstarter.zip"

function Download-File {
param (
  [string]$url,
  [string]$file
 )
  Write-Host "Downloading $url to $file"
  $downloader = new-object System.Net.WebClient
  $downloader.DownloadFile($url, $file)
}

function Install-BoxStarter {
  $choice = ""
  while ($choice -notmatch "[y|n]") {
     $choice = read-host "About to install BoxStarter. Do you want to continue? (y/N)"
  }

  if ($choice -ne "y") {
     Write-Host "Quitting. Nothing was done..."
     exit
  }

  # download the package
  Download-File $url $file

  # download 7zip
  Write-Host "Download 7Zip commandline tool"
  $7zaExe = Join-Path $tempDir '7za.exe'

  Download-File 'http://chocolatey.org/7za.exe' "$7zaExe"

  # unzip the package
  Write-Host "Extracting $file to $tempDir..."
  Start-Process "$7zaExe" -ArgumentList "x -o`"$tempDir`" -y `"$file`"" -Wait

  # call chocolatey install
  Write-Host "Installing BoxStarter on this machine"
  $boxStarterInstallBat = Join-Path $tempDir "setup.bat"

  & $boxStarterInstallBat " -Force"
}

$boxstarter = get-module -listavailable | where-object { $_.name -eq "Boxstarter.Common" }
if ($boxstarter -eq $null) {
    Install-BoxStarter
}

Write-Host "Setting local development root directory"
## TODO: check if environment variable is already set.
$devRootDir = ""
while ($devRootDir -eq "") {

   $devRootDir = Read-Host "Where is your local development root, e.g. c:\dev ?"

   while (![System.IO.Directory]::Exists($devRootDir)) {
      $createdevRootDir = Read-Host "Local development root does not exist. Do you want to create it? (y/N/q)"

      if ($createdevRootDir -eq "y") {
         [System.IO.Directory]::CreateDirectory($devRootDir)
      } elseif ($createdevRootDir -eq "q") {
         Write-Host "Quitting"
         exit
      } else {
         $devRootDir = ""
      }
   }
}

# Set DEVROOT env var for USER
setx DEVROOT $devRootDir

$boxStarterPackageDir = Join-Path $devRootDir "\provisioning\windows\BoxStarter Packages\"

while (![System.IO.Directory]::Exists($boxStarterPackageDir)) {
   $createBoxStarterPackageDir = read-host "BoxStarter package location does not exist. Do you want to create it? (y/q)"

   if ($createBoxStarterPackageDir -eq "y") {
      [System.IO.Directory]::CreateDirectory($boxStarterPackageDir)
   } elseif ($createBoxStarterPackageDir -eq "q") {
         Write-Host "Quitting"
         exit
   }
}

Write-Host "Setting BoxStarter standard configuration"
$cmd = 'Import-Module BoxStarter.Chocolatey; Set-BoxStarterConfig -LocalRepo "$boxStarterPackageDir"'
powershell -NoProfile -ExecutionPolicy unrestricted -Command $cmd

# Set Chocolatey path if not already done
$chocInstallVariableName = "ChocolateyInstall"
$chocoPath = [Environment]::GetEnvironmentVariable($chocInstallVariableName, [System.EnvironmentVariableTarget]::User)
$chocoExePath = 'C:\ProgramData\Chocolatey\bin'
if ($chocoPath -ne $null) {
  $chocoExePath = Join-Path $chocoPath 'bin'
}

if ($($env:Path).ToLower().Contains($($chocoExePath).ToLower()) -eq $false) {
  $env:Path = [Environment]::GetEnvironmentVariable('Path',[System.EnvironmentVariableTarget]::Machine);
}

Write-Host "Done!"
