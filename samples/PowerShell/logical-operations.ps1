function Test-Boolean ([bool] $a, [bool] $b) {
    Write-Host "A and B:   " ($a -and $b)
    Write-Host "A or B:    " ($a -or $b)
    Write-Host "not A:     " (-not $a)
    Write-Host "not A:     " (!$a)
    Write-Host "A xor B:   " ($a -xor $b)
}
