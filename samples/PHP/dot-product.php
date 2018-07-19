<?php
function dot_product($v1, $v2) {
  if (count($v1) != count($v2))
    throw new Exception('Arrays have different lengths');
  return array_sum(array_map('bcmul', $v1, $v2));
}

echo dot_product(array(1, 3, -5), array(4, -2, -1)), "\n";
?>
