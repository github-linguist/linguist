<?php
function mode($arr) {
    $count = array_count_values($arr);
    $best = max($count);
    return array_keys($count, $best);
}

print_r(mode(array(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17)));
print_r(mode(array(1, 1, 2, 4, 4)));
?>
