// arr is an array of string to int. any type can be used in both places.
var keys: domain(string);
var arr: [keys] int;

// keys can be added to a domain using +, new values will be initialized to the default value (0 for int)
keys += "foo";
keys += "bar";
keys += "baz";

// array access via [] or ()
arr["foo"] = 1;
arr["bar"] = 4;
arr("baz") = 6;

// write auto-formats domains and arrays
writeln("Keys: ", keys);
writeln("Values: ", arr);

// keys can be deleted using -
keys -= "bar";

writeln("Keys: ", keys);
writeln("Values: ", arr);

// chapel also supports array literals
var arr2 = [ "John" => 3, "Pete" => 14 ];

writeln("arr2 keys: ", arr2.domain);
writeln("arr2 values: ", arr2);
