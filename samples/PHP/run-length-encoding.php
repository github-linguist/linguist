<?php
function encode($str) {
  return preg_replace('/(.)\1*/e', 'strlen($0) . $1', $str);
}

function decode($str) {
  return preg_replace('/(\d+)(\D)/e', 'str_repeat($2, $1)', $str);
}

echo encode('WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'), "\n";
echo decode('12W1B12W3B24W1B14W'), "\n";
?>
