function pow($a, [int]$b) {
    if ($b -eq -1) { return 1/$a }
    if ($b -eq 0)  { return 1 }
    if ($b -eq 1)  { return $a }
    if ($b -lt 0) {
        $rec = $true # reciprocal needed
        $b = -$b
    }

    $result = $a
    2..$b | ForEach-Object {
        $result *= $a
    }

    if ($rec) {
        return 1/$result
    } else {
        return $result
    }
}
