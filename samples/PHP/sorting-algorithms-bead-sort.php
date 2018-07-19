<?php
function columns($arr) {
    if (count($m) == 0)
        return array();
    else if (count($m) == 1)
        return array_chunk($m[0], 1);

    array_unshift($arr, NULL);
    // array_map(NULL, $arr[0], $arr[1], ...)
    $transpose = call_user_func_array('array_map', $arr);
    return array_map('array_filter', $transpose);
}

function beadsort($arr) {
    foreach ($arr as $e)
        $poles []= array_fill(0, $e, 1);
    return array_map('count', columns(columns($poles)));
}

print_r(beadsort(array(5,3,1,7,4,1,1)));
?>
