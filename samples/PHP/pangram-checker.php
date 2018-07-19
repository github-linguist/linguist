function isPangram($text) {
    foreach (str_split($text) as $c) {
        if ($c >= 'a' && $c <= 'z')
            $bitset |= (1 << (ord($c) - ord('a')));
        else if ($c >= 'A' && $c <= 'Z')
            $bitset |= (1 << (ord($c) - ord('A')));
    }
    return $bitset == 0x3ffffff;
}

$test = array(
    "the quick brown fox jumps over the lazy dog",
    "the quick brown fox jumped over the lazy dog",
    "ABCDEFGHIJKLMNOPQSTUVWXYZ",
    "ABCDEFGHIJKL.NOPQRSTUVWXYZ",
    "ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ"
);

foreach ($test as $str)
    echo "$str : ", isPangram($str) ? 'T' : 'F', '</br>';
