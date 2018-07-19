<?php
class Example {
  function foo($x) {
    return 42 + $x;
  }
}

$example = new Example();

$name = 'foo';
echo $example->$name(5), "\n";        // prints "47"

// alternately:
echo call_user_func(array($example, $name), 5), "\n";
?>
