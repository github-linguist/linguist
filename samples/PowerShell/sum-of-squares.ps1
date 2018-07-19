function Get-SquareSum ($a) {
    if ($a.Length -eq 0) {
        return 0
    } else {
        $x = $a `
             | ForEach-Object { $_ * $_ } `
             | Measure-Object -Sum
        return $x.Sum
    }
}
