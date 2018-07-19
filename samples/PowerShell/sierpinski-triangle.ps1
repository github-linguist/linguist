function triangle($o) {
    $n = [Math]::Pow(2, $o)
    $line = ,' '*(2*$n+1)
    $line[$n] = '█'
    $OFS = ''
    for ($i = 0; $i -lt $n; $i++) {
        Write-Host $line
        $u = '█'
        for ($j = $n - $i; $j -lt $n + $i + 1; $j++) {
            if ($line[$j-1] -eq $line[$j+1]) {
                $t = ' '
            } else {
                $t = '█'
            }
            $line[$j-1] = $u
            $u = $t
        }
        $line[$n+$i] = $t
        $line[$n+$i+1] = '█'
    }
}
