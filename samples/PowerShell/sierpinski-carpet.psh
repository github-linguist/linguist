function inCarpet($x, $y) {
    while ($x -ne 0 -and $y -ne 0) {
        if ($x % 3 -eq 1 -and $y % 3 -eq 1) {
            return " "
        }
        $x = [Math]::Truncate($x / 3)
        $y = [Math]::Truncate($y / 3)
    }
    return "█"
}

function carpet($n) {
    for ($y = 0; $y -lt [Math]::Pow(3, $n); $y++) {
        for ($x = 0; $x -lt [Math]::Pow(3, $n); $x++) {
            Write-Host -NoNewline (inCarpet $x $y)
        }
        Write-Host
    }
}
