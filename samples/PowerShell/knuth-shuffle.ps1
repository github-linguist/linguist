function shuffle ($a) {
    $c = $a.Clone()  # make copy to avoid clobbering $a
    1..($c.Length - 1) | ForEach-Object {
        $i = Get-Random -Minimum $_ -Maximum $c.Length
        $c[$_-1],$c[$i] = $c[$i],$c[$_-1]
        $c[$_-1]  # return newly-shuffled value
    }
    $c[-1]  # last value
}
