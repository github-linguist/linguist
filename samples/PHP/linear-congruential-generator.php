<?php
function bsd_rand($seed) {
    return function() use (&$seed) {
        return $seed = (1103515245 * $seed + 12345) % (1 << 31);
    };
}

function msvcrt_rand($seed) {
    return function() use (&$seed) {
        return ($seed = (214013 * $seed + 2531011) % (1 << 31)) >> 16;
    };
}

$lcg = bsd_rand(0);
echo "BSD ";
for ($i = 0; $i < 10; $i++)
    echo $lcg(), " ";
echo "\n";

$lcg = msvcrt_rand(0);
echo "Microsoft ";
for ($i = 0; $i < 10; $i++)
    echo $lcg(), " ";
echo "\n";
?>
