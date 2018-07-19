echo 'M929 has a factor: ',  mersenneFactor(929), '</br>';

function mersenneFactor($p) {
    $limit = sqrt(pow(2, $p) - 1);
    for ($k = 1; 2 * $p * $k - 1 < $limit; $k++) {
        $q = 2 * $p * $k + 1;
        if (isPrime($q) && ($q % 8 == 1 || $q % 8 == 7) && bcpowmod("2", "$p", "$q") == "1") {
            return $q;
        }
    }
    return 0;
}

function isPrime($n) {
    if ($n < 2 || $n % 2 == 0) return $n == 2;
    for ($i = 3; $i * $i <= $n; $i += 2) {
        if ($n % $i == 0) {
            return false;
        }
    }
    return true;
}
