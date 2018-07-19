<?php

echo "Enter an integer [int1]: ";
fscanf(STDIN, "%d\n", $int1);
if(!is_numeric($int1)) {
  echo "Invalid input; terminating.\n";
  exit(1);      // return w/ general error
}

echo "Enter an integer [int2]: ";
fscanf(STDIN, "%d\n", $int2);
if(!is_numeric($int2)) {
  echo "Invalid input; terminating.\n";
  exit(1);      // return w/ general error
}

// now $int1 and $int2 are numbers.
// for simplicity, this does not explicitly examine types

if($int1 < $int2)
  echo "int1 < int2\n";
if($int1 == $int2)
  echo "int1 = int2\n";
if($int1 > $int2)
  echo "int1 > int2\n";

?>
