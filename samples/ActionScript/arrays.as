//creates an array of length 10
var array1:Array = new Array(10);
//creates an array with the values 1, 2
var array2:Array = new Array(1,2);
//arrays can also be set using array literals
var array3:Array = ["foo", "bar"];
//to resize an array, modify the length property
array2.length = 3;
//arrays can contain objects of multiple types.
array2[2] = "Hello";
//get a value from an array
trace(array2[2]);
//append a value to an array
array2.push(4);
//get and remove the last element of an array
trace(array2.pop());
