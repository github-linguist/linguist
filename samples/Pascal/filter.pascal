const

numbers:array[0..9] of integer = (0,1,2,3,4,5,6,7,8,9);

for x = 1 to 10 do
     if odd(numbers[x]) then
         writeln( 'The number ',numbers[x],' is odd.');
     else
         writeln( 'The number ',numbers[x],' is even.');
