var add, a, b;
add = argument0; // get the string with the numbers to add
a = real(string_copy(add, 1, string_pos(" ", add)));
b = real(string_copy(add, string_pos(" ", add) + 1, string_length(add) - string_pos(" ", add)));
return(a + b);
