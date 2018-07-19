<?php
function lookandsay($str) {
  return preg_replace('/(.)\1*/e', 'strlen($0) . $1', $str);
}

$num = "1";
foreach (range(1,10) as $i) {
  echo "$num\n";
  $num = lookandsay($num);
}
?>
