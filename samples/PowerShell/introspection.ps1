# version is found in $PSVersionTable
if ($PSVersionTable['PSVersion'] -lt '2.0') {
    exit
}

if ((Test-Path Variable:bloop) -and ([Math]::Abs)) {
    [Math]::Abs($bloop)
}

# find integer variables and their sum
Get-Variable -Scope global `
    | Where-Object { $_.Value -is [int] } `
    | Measure-Object -Sum Value `
    | Select-Object Count,Sum
