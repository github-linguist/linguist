param virtualMachineSize string
param adminUsername string

@secure()
param adminPassword string

param storageAccountType string
param location string = resourceGroup().location

var nic1Name = 'nic-1'
var nic2Name = 'nic-2'
var virtualNetworkName = 'virtualNetwork'
var subnet1Name = 'subnet-1'
var subnet2Name = 'subnet-2'
var publicIPAddressName = 'publicIp'
var diagStorageAccountName = concat('diags', uniqueString(resourceGroup().id))
var networkSecurityGroupName = 'NSG'
var networkSecurityGroupName2 = concat(subnet2Name, '-nsg')

module vmMod './vm.bicep' = {
  name: 'vmMod'
  params: {
    adminUsername: adminUsername
    adminPassword: adminPassword
    virtualMachineSize: virtualMachineSize
    virtualMachineName: 'VM-MultiNic'
    nic1Id: nic1.id
    nic2Id: nic2.id
    diagsStorageUri: diagsAccount.properties.primaryEndpoints.blob
  }
}

resource diagsAccount 'Microsoft.Storage/storageAccounts@2017-06-01' = {
  name: diagStorageAccountName
  location: location
  sku: {
    name: storageAccountType
  }
  kind: 'Storage'
}

// Simple Network Security Group for subnet2
resource nsg2 'Microsoft.Network/networkSecurityGroups@2019-08-01' = {
  name: networkSecurityGroupName2
  location: location
  properties: {}
}

// This will build a Virtual Network.
resource vnet 'Microsoft.Network/virtualNetworks@2017-06-01' = {
  name: virtualNetworkName
  location: location
  properties: {
    addressSpace: {
      addressPrefixes: [
        '10.0.0.0/16'
      ]
    }
    subnets: [
      {
        name: subnet1Name
        properties: {
          addressPrefix: '10.0.0.0/24'
        }
      }
      {
        name: subnet2Name
        properties: {
          addressPrefix: '10.0.1.0/24'
          networkSecurityGroup: {
            id: nsg2.id
          }
        }
      }
    ]
  }
}

// This will be your Primary NIC
resource nic1 'Microsoft.Network/networkInterfaces@2017-06-01' = {
  name: nic1Name
  location: location
  properties: {
    ipConfigurations: [
      {
        name: 'ipconfig1'
        properties: {
          subnet: {
            id: '${vnet.id}/subnets/${subnet1Name}'
          }
          privateIPAllocationMethod: 'Dynamic'
          publicIPAddress: {
            id: pip.id
          }
        }
      }
    ]
    networkSecurityGroup: {
      id: nsg.id
    }
  }
}

// This will be your Secondary NIC
resource nic2 'Microsoft.Network/networkInterfaces@2017-06-01' = {
  name: nic2Name
  location: location
  properties: {
    ipConfigurations: [
      {
        name: 'ipconfig1'
        properties: {
          subnet: {
            id: '${vnet.id}/subnets/${subnet2Name}'
          }
          privateIPAllocationMethod: 'Dynamic'
        }
      }
    ]
  }
}

// Public IP for your Primary NIC
resource pip 'Microsoft.Network/publicIPAddresses@2017-06-01' = {
  name: publicIPAddressName
  location: location
  properties: {
    publicIPAllocationMethod: 'Dynamic'
  }
}

// Network Security Group (NSG) for your Primary NIC
resource nsg 'Microsoft.Network/networkSecurityGroups@2016-09-01' = {
  name: networkSecurityGroupName
  location: location
  properties: {
    securityRules: [
      {
        name: 'default-allow-rdp'
        properties: {
          priority: 1000
          sourceAddressPrefix: '*'
          protocol: 'Tcp'
          destinationPortRange: '3389'
          access: 'Allow'
          direction: 'Inbound'
          sourcePortRange: '*'
          destinationAddressPrefix: '*'
        }
      }
    ]
  }
}

output publicIp string = pip.properties.ipAddress
