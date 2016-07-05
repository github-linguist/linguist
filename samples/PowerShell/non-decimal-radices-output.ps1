foreach ($n in 0..33) {
    "Base 2:  " + [Convert]::ToString($n, 2)
    "Base 8:  " + [Convert]::ToString($n, 8)
    "Base 10: " + $n
    "Base 10: " + [Convert]::ToString($n, 10)
    "Base 10: " + ("{0:D}" -f $n)
    "Base 16: " + [Convert]::ToString($n, 16)
    "Base 16: " + ("{0:X}" -f $n)
}
