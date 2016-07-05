function isPrime ($n) {
    if ($n -eq 1) {
        return $false
    } else {
        return (@(2..[Math]::Sqrt($n) | Where-Object { $n % $_ -eq 0 }).Length -eq 0)
    }
}
