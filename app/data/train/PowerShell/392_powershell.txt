Param (		
		[Parameter(Mandatory=$false)]
		[string]$BuildFlavor = "Release"
    )


$buildFolderPath = $PSScriptRoot

if($env:APPVEYOR_BUILD_FOLDER -ne $null)
{
     $buildFolderPath = $env:APPVEYOR_BUILD_FOLDER
}

Write-Host "Build dir : $buildFolderPath"

$computeClientXmlPath = Join-Path $buildFolderPath   "ComputeClient\Compute.Client\bin\$BuildFlavor\DD.CBU.Compute.Api.Client.xml"
$computeClientContractsXmlPath = Join-Path $buildFolderPath   "ComputeClient\Compute.Contracts\bin\$BuildFlavor\DD.CBU.Compute.Api.Contracts.xml"
$xmlDocXslPath = Join-Path $buildFolderPath  "xmldoc2md\xmldoc2md.xsl"
$computeClientMdPath  = Join-Path $buildFolderPath  "docs\DD.CBU.Compute.Api.Client.md"
$computeClientContractsMdPath  = Join-Path $buildFolderPath  "docs\DD.CBU.Compute.Api.Contracts.md"

.\xmldoc2md\xmldoc2md.ps1 -xml $computeClientXmlPath  -xsl $xmlDocXslPath  -output $computeClientMdPath  
.\xmldoc2md\xmldoc2md.ps1 -xml $computeClientContractsXmlPath -xsl $xmlDocXslPath  -output $computeClientContractsMdPath 