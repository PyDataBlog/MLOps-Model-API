#Login Azure 
#Create Resource Group
#Create Storage
#Create Network
#Create VM's

#Login to Azure
Login-AzureRmAccount

# Create Resource Group.
 New-AzureRmResourceGroup -Name 'New Vnet with subnets' -Location 'westeurope'

#Create Storage
 $storage = New-azurestorage

 # Create a virtual network
 $Vnet=New-AzureRmVirtualNetwork `
  -ResourceGroupName MyResourceGroup `
  -Location westeurope `
  -Name MyVnet `
  -AddressPrefix 192.168.0.0/16 `
  -Subnet $Subnet1,$Subnet2
  
# Create Subnet 1
$Subnet1 = New-AzureRmVirtualNetworkSubnetConfig `
  -Name Public `
  -AddressPrefix 192.168.0.0/16
# Create Subnet 2
$Subnet2 = New-AzureRmVirtualNetworkSubnetConfig `
  -Name Private `
  -AddressPrefix 192.168.0.0/16

  $storage = New-azurestorage



  
