<?php
$words = explode("\n", file_get_contents('http://www.puzzlers.org/pub/wordlists/unixdict.txt'));
foreach ($words as $word) {
    $chars = str_split($word);
    sort($chars);
    $anagram[implode($chars)][] = $word;
}

$best = max(array_map('count', $anagram));
foreach ($anagram as $ana)
    if (count($ana) == $best)
        print_r($ana);
?>
