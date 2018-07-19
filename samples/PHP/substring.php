<?php
$str = 'abcdefgh';
$n = 2;
$m = 3;
echo substr($str, $n, $m), "\n"; //cde
echo substr($str, $n), "\n"; //cdefgh
echo substr($str, 0, -1), "\n"; //abcdefg
echo substr($str, strpos($str, 'd'), $m), "\n"; //def
echo substr($str, strpos($str, 'de'), $m), "\n"; //def
?>
