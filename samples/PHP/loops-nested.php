<?php
for ($i = 0; $i < 10; $i++)
    for ($j = 0; $j < 10; $j++)
        $a[$i][$j] = rand(1, 20);

foreach ($a as $row) {
    foreach ($row as $element) {
        echo " $element";
        if ($element == 20)
            break 2; // 2 is the number of loops we want to break out of
    }
    echo "\n";
}
echo "\n";
?>
