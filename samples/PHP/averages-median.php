function median($arr)
{
    sort($arr);
    $count = count($arr); //count the number of values in array
    $middleval = floor(($count-1)/2); // find the middle value, or the lowest middle value
    if ($count % 2) { // odd number, middle is the median
        $median = $arr[$middleval];
    } else { // even number, calculate avg of 2 medians
        $low = $arr[$middleval];
        $high = $arr[$middleval+1];
        $median = (($low+$high)/2);
    }
    return $median;
}

echo median(array(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) . "\n";  // 4.4
echo median(array(4.1, 7.2, 1.7, 9.3, 4.4, 3.2)) . "\n";       // 4.25
