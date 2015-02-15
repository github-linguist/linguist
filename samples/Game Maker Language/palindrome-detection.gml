//Setting a var from an argument passed to the script
var str;
str = argument0
//Takes out all spaces/anything that is not a letter or a number and turns uppercase letters to lowercase
str = string_lettersdigits(string_lower(string_replace(str,' ','')));
var inv;
inv = '';
//for loop that reverses the sequence
var i;
for (i = 0; i < string_length(str); i += 1;)
    {
    inv += string_copy(str,string_length(str)-i,1);
    }
//returns true if the sequence is a palindrome else returns false
return (str == inv);
