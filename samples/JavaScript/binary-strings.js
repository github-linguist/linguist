//String creation
var str='';
//or
str2=new String();


//String assignment
str="Hello";
//or
str2=', Hey there'; //can use " or '
str=str+str2;//concantenates
//string deletion
delete str2;//this will return true or false, true when it has been successfully deleted, it shouldn't/won't work when the variable has been declared with the keyword 'var', you don't have to initialize variables with the var keyword in JavaScript, but when you do, you cannot 'delete' them. However JavaScript garbage collects, so when the function returns, the variable declared on the function is erased.

//String comparison
str!=="Hello"; //!== not equal-> returns true there's also !===
str=="Hello, Hey there"; //returns true
//compares 'byte' by 'byte'
"Character Z">"Character A"; //returns true, when using > or < operators it converts the string to an array and evalues the first index that is higher than another. (using unicode values) in this case 'Z' char code is 90 (decimal) and 'A' char code is 65, therefore making one string "larger" than the other.

//String cloning and copying
string=str;//Strings are immutable therefore when you assign a string to a variable another one is created. So for two variables to have the 'same' string you have to add that string to an object, and get/set the string from that object

//Check if a string is empty
Boolean(''); //returns false
''[0]; //returns undefined
''.charCodeAt(); //returns NaN
''==0; //returns true
''===0; //returns false
''==false; //returns true

//Append byte to String
str+="\x40";//using + operator before the equal sign on a string makes it equal to str=str+"\x40"

//Extract a substring from a string
//str is "Hello, Hey there@"
str.substr(3); //returns "lo, Hey there@"
str.substr(-5); //returns "here@" negative values just go to the end
str.substr(7,9); //returns "Hey there" index of 7 + 9 characters after the 7
str.substring(3); //same as substr
str.substring(-5); //negative values don't work on substring same as substr(0)
str.substring(7,9); //returns "He" that is, whatever is between index 7 and index 9, same as substring(9,7)

//Replace every occurence of x byte with another string
str3="url,url,url,url,url";
str3.replace(/,/g,'\n') //Regex ,returns the same string with the , replaced by \n
str4=str3.replace(/./g,function(index){//it also supports callback functions, the function will be called when a match has been found..
return index==','?'\n':index;//returns replacement
})

//Join Strings
[str," ",str3].join(" "/*this is the character that will glue the strings*/)//we can join an array of strings
str3+str4;
str.concat('\n',str4); //concantenate them
