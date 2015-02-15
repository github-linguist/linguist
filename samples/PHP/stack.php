$stack = array();

empty( $stack ); // true

array_push( $stack, 1 ); // or $stack[] = 1;
array_push( $stack, 2 ); // or $stack[] = 2;

empty( $stack ); // false

echo array_pop( $stack ); // outputs "2"
echo array_pop( $stack ); // outputs "1"
