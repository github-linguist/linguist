var hello:String = "Hello,How,Are,You,Today";
var tokens:Array = hello.split(",");
trace(tokens.join("."));

// Or as a one-liner
trace("Hello,How,Are,You,Today".split(",").join("."));
