// declare test string
string s = "Hello,How,Are,You,Today";
// create array of strings, could use var words instead if desired
string[] words = s.split(",");
// create string by joining array of strings with .
string joined = string.joinv(".", words);
