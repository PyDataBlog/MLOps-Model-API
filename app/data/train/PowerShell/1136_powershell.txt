Param (
  [Parameter()]
  [String]$SAKey,
  [String]$SAName
)
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
Install-PackageProvider -Name NuGet -MinimumVersion 2.8.5.201 -Force
Install-Module Azure -Confirm:$False
Import-Module Azure
$storageContext = New-AzureStorageContext -StorageAccountName $SAName -StorageAccountKey $SAKey
$storageContext |  New-AzureStorageShare -Name websites
$storageContext |  New-AzureStorageShare -Name m2m
$storageContext |  New-AzureStorageShare -Name certificates