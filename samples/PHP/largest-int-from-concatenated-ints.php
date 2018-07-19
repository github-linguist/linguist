function maxnum($nums) {
    usort($nums,  function ($x, $y) { return strcmp("$y$x", "$x$y"); });
    return implode('', $nums);
}

echo maxnum(array(1, 34, 3, 98, 9, 76, 45, 4)), "\n";
echo maxnum(array(54, 546, 548, 60)), "\n";
