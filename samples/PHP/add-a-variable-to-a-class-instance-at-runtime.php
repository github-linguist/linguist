class E {};

$e=new E();

$e->foo=1;

$e->{"foo"} = 1; // using a runtime name
$x = "foo";
$e->$x = 1; // using a runtime name in a variable
