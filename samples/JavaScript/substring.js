var str = "abcdefgh";

var n = 2;
var m = 3;

//  *  starting from n characters in and of m length;
str.substr(n, m);  // => "cde"

//  * starting from n characters in, up to the end of the string;
str.substr(n);  // => "cdefgh"
str.substring(n);  // => "cdefgh"

//  * whole string minus last character;
str.substring(0, str.length - 1);  // => "abcdefg"

//  * starting from a known character within the string and of m length;
str.substr(str.indexOf('b'), m);  // => "bcd"

//  * starting from a known substring within the string and of m length.
str.substr(str.indexOf('bc'), m);  // => "bcd"
