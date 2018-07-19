<?php
// Read dictionary into array
$dictionary = array_fill_keys(file(
    'http://www.puzzlers.org/pub/wordlists/unixdict.txt',
    FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES
), true);
foreach (array_keys($dictionary) as $word) {
    $reversed_word = strrev($word);
    if (isset($dictionary[$reversed_word]) && $word > $reversed_word)
        $words[$word] = $reversed_word;
}
echo count($words), "\n";
// array_rand() returns keys, not values
foreach (array_rand($words, 5) as $word)
    echo "$word $words[$word]\n";
