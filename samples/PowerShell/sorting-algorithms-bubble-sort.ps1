function bubblesort ($a) {
    $l = $a.Length
    $hasChanged = $true
    while ($hasChanged) {
        $hasChanged = $false
        $l--
        for ($i = 0; $i -lt $l; $i++) {
            if ($a[$i] -gt $a[$i+1]) {
                $a[$i], $a[$i+1] = $a[$i+1], $a[$i]
                $hasChanged = $true
            }
        }
    }
}
