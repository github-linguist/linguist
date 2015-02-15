// Create a new array with length 0
var myArray = new Array();

// Create a new array with length 5
var myArray1 = new Array(5);

// Create an array with 2 members (length is 2)
var myArray2 = new Array("Item1","Item2");

// Create an array with 2 members using an array literal
var myArray3 = ["Item1", "Item2"];

// Assign a value to member [2] (length is now 3)
myArray[2] = 5;

var x = myArray[2] + myArray.length;   // 8

// Elisions are supported, but are buggy in some implementations
var y = [0,1,,];  // length 3, or 4 in buggy implementations
