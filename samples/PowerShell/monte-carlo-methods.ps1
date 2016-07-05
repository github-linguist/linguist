function Get-Pi ($Iterations = 10000) {
    $InCircle = 0
    for ($i = 0; $i -lt $Iterations; $i++) {
        $x = Get-Random 1.0
        $y = Get-Random 1.0
        if ([Math]::Sqrt($x * $x + $y * $y) -le 1) {
            $InCircle++
        }
    }
    $Pi = [decimal] $InCircle / $Iterations * 4
    $RealPi = [decimal] "3.141592653589793238462643383280"
    $Diff = [Math]::Abs(($Pi - $RealPi) / $RealPi * 100)
    New-Object PSObject `
        | Add-Member -PassThru NoteProperty Iterations $Iterations `
        | Add-Member -PassThru NoteProperty Pi $Pi `
        | Add-Member -PassThru NoteProperty "% Difference" $Diff
}
