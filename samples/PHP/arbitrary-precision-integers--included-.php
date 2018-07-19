<?php
$y = bcpow('5', bcpow('4', bcpow('3', '2')));
printf("5**4**3**2 = %s...%s and has %d digits\n", substr($y,0,20), substr($y,-20), strlen($y));
?>
