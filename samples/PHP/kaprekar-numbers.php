set_time_limit(300);

print_r(array_filter(range(1, 10000), 'isKaprekar'));
echo count(array_filter(range(1, 1000000), 'isKaprekar'));

function isKaprekar($n) {
    $a = $n * $n;
    $b = bcmod("$a", "10");
    for ($d = 1, $t = 0; $a > 0; $d *= 10) {
        $b += $t * $d;
        if ($b > $n) break;
        $a = floor($a / 10);
        if ($b && $a + $b == $n)
            return true;
        $t = bcmod("$a", "10");
    }
    return false;
}
