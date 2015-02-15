<?php
class Example {
  function foo() {
    echo "this is foo\n";
  }
  function bar() {
    echo "this is bar\n";
  }
  function __call($name, $args) {
    echo "tried to handle unknown method $name\n";
    if ($args)
      echo "it had arguments: ", implode(', ', $args), "\n";
  }
}

$example = new Example();

$example->foo();        // prints "this is foo"
$example->bar();        // prints "this is bar"
$example->grill();      // prints "tried to handle unknown method grill"
$example->ding("dong"); // prints "tried to handle unknown method ding"
                        // prints "it had arguments: dong
?>
