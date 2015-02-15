echo lcm(12, 18) == 36;

function lcm($m, $n) {
    if ($m == 0 || $n == 0) return 0;
    $r = ($m * $n) / gcd($m, $n);
    return abs($r);
}

function gcd($a, $b) {
    while ($b != 0) {
        $t = $b;
        $b = $a % $b;
        $a = $t;
    }
    return $a;
}
