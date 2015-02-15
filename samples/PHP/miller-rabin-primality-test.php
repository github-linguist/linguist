<?php
function is_prime($n, $k) {
    if ($n == 2)
        return true;
    if ($n < 2 || $n % 2 == 0)
        return false;

    $d = $n - 1;
    $s = 0;

    while ($d % 2 == 0) {
        $d /= 2;
        $s++;
    }

    for ($i = 0; $i < $k; $i++) {
        $a = rand(2, $n-1);

        $x = bcpowmod($a, $d, $n);
        if ($x == 1 || $x == $n-1)
            continue;

        for ($j = 1; $j < $s; $j++) {
            $x = bcmod(bcmul($x, $x), $n);
            if ($x == 1)
                return false;
            if ($x == $n-1)
                continue 2;
        }
        return false;
    }
    return true;
}

for ($i = 1; $i <= 1000; $i++)
    if (is_prime($i, 10))
        echo "$i, ";
echo "\n";
?>
