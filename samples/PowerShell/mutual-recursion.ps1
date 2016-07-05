function F($n) {
    if ($n -eq 0) { return 1 }
    return $n - (M (F ($n - 1)))
}

function M($n) {
    if ($n -eq 0) { return 0 }
    return $n - (F (M ($n - 1)))
}
