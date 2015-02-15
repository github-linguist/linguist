<?php
$a = fgets(STDIN);
$b = fgets(STDIN);

echo
    "sum:                 ", $a + $b, "\n",
    "difference:          ", $a - $b, "\n",
    "product:             ", $a * $b, "\n",
    "truncating quotient: ", (int)($a / $b), "\n",
    "flooring quotient:   ", floor($a / $b), "\n",
    "remainder:           ", $a % $b, "\n";
?>
