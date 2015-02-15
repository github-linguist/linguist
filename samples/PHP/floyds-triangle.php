<?php
floyds_triangle(5);
floyds_triangle(14);

function floyds_triangle($n) {
    echo "n = " . $n . "\r\n";

    for($r = 1, $i = 1, $c = 0; $r <= $n; $i++) {
        $cols = ceil(log10($n*($n-1)/2 + $c + 2));
        printf("%".$cols."d ", $i);
        if(++$c == $r) {
            echo "\r\n";
            $r++;
            $c = 0;
        }
    }
?>
