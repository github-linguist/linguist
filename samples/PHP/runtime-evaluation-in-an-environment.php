<?php
function eval_with_x($code, $a, $b) {
    $x = $a;
    $first = eval($code);
    $x = $b;
    $second = eval($code);
    return $second - $first;
}

echo eval_with_x('return 3 * $x;', 5, 10), "\n"; # Prints "15".
?>
