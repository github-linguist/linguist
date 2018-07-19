<?php
$a = array();
# add elements "at the end"
array_push($a, 55, 10, 20);
print_r($a);
# using an explicit key
$a['one'] = 1;
$a['two'] = 2;
print_r($a);
?>
