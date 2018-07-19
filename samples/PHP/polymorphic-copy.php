<?php
class T {
      function name() { return "T"; }
}

class S {
      function name() { return "S"; }
}

$obj1 = new T();
$obj2 = new S();
$obj3 = clone $obj1;
$obj4 = clone $obj2;
echo $obj3->name(), "\n"; // prints "T"
echo $obj4->name(), "\n"; // prints "S"
?>
