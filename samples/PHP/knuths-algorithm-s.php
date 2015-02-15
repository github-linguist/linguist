<?php
function s_of_n_creator($n) {
    $sample = array();
    $i = 0;
    return function($item) use (&$sample, &$i, $n) {
        $i++;
        if ($i <= $n) {
            // Keep first n items
            $sample[] = $item;
        } else if (rand(0, $i-1) < $n) {
            // Keep item
            $sample[rand(0, $n-1)] = $item;
        }
        return $sample;
    };
}

$items = range(0, 9);

for ($trial = 0; $trial < 100000; $trial++) {
    $s_of_n = s_of_n_creator(3);
    foreach ($items as $item)
        $sample = $s_of_n($item);
    foreach ($sample as $s)
        $bin[$s]++;
}
print_r($bin);
?>
