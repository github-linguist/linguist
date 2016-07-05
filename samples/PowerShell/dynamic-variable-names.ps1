$variableName = Read-Host
New-Variable $variableName 'Foo'
Get-Variable $variableName
