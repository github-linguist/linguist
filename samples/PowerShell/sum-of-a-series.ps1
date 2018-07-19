$x = 1..1000 `
       | ForEach-Object { 1 / ($_ * $_) } `
       | Measure-Object -Sum
Write-Host Sum = $x.Sum
