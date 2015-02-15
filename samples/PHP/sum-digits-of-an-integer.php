<?php
function sumDigits($num, $base = 10) {
    $s = base_convert($num, 10, $base);
    foreach (str_split($s) as $c)
        $result += intval($c, $base);
    return $result;
}
echo sumDigits(1), "\n";
echo sumDigits(12345), "\n";
echo sumDigits(123045), "\n";
echo sumDigits(0xfe, 16), "\n";
echo sumDigits(0xf0e, 16), "\n";
?>
