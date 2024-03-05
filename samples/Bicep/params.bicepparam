using 'br/public:ai/cognitiveservices:1.1.1'

var suffix = 'ac9h8d'

param skuName = 'S0'
param kind = 'OpenAI'
param name = 'openai-${suffix}'
param location = 'westus2'
param deployments = [
  {
    name: 'model-deployment-${suffix}'
    sku: {
      name: 'Standard'
      capacity: 120
    }
    properties: {
      model: {
        format: 'OpenAI'
        name: 'text-davinci-002'
        version: 1
      }
      raiPolicyName: 'Microsoft.Default'
    }
  }
]
