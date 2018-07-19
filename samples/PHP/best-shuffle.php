foreach (split(' ', 'abracadabra seesaw pop grrrrrr up a') as $w)
    echo bestShuffle($w) . '<br>';

function bestShuffle($s1) {
    $s2 = str_shuffle($s1);
    for ($i = 0; $i < strlen($s2); $i++) {
        if ($s2[$i] != $s1[$i]) continue;
        for ($j = 0; $j < strlen($s2); $j++)
            if ($i != $j && $s2[$i] != $s1[$j] && $s2[$j] != $s1[$i]) {
                $t = $s2[$i];
                $s2[$i] = $s2[$j];
                $s2[$j] = $t;
                break;
            }
    }
    return "$s1 $s2 " . countSame($s1, $s2);
}

function countSame($s1, $s2) {
    $cnt = 0;
    for ($i = 0; $i < strlen($s2); $i++)
        if ($s1[$i] == $s2[$i])
            $cnt++;
    return "($cnt)";
}
