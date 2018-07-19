// Using an array/hash
$fruits = array( "apple", "banana", "cherry" );
$fruits = array( "apple" => 0, "banana" => 1, "cherry" => 2 );

// If you are inside a class scope
class Fruit {
  const APPLE = 0;
  const BANANA = 1;
  const CHERRY = 2;
}

// Then you can access them as such
$value = Fruit::APPLE;

// Or, you can do it using define()
define("FRUIT_APPLE", 0);
define("FRUIT_BANANA", 1);
define("FRUIT_CHERRY", 2);
