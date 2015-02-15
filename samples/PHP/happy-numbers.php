function isHappy($n) {
    while (1) {
        $total = 0;
        while ($n > 0) {
            $total += pow(($n % 10), 2);
            $n /= 10;
        }
        if ($total == 1)
            return true;
        if (array_key_exists($total, $past))
            return false;
        $n = $total;
        $past[$total] = 0;
    }
}

$i = $cnt = 0;
while ($cnt < 8) {
    if (isHappy($i)) {
        echo "$i ";
        $cnt++;
    }
    $i++;
}
