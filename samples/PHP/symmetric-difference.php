<?php
$a = array('John', 'Bob', 'Mary', 'Serena');
$b = array('Jim', 'Mary', 'John', 'Bob');

// Remove any duplicates
$a = array_unique($a);
$b = array_unique($b);

// Get the individual differences, using array_diff()
$a_minus_b = array_diff($a, $b);
$b_minus_a = array_diff($b, $a);

// Simply merge them together to get the symmetric difference
$symmetric_difference = array_merge($a_minus_b, $b_minus_a);

// Present our results.
echo 'List A:               ', implode(', ', $a),
   "\nList B:               ", implode(', ', $b),
  "\nA \\ B:                ", implode(', ', $a_minus_b),
  "\nB \\ A:                ", implode(', ', $b_minus_a),
   "\nSymmetric difference: ", implode(', ', $symmetric_difference), "\n";
?>
