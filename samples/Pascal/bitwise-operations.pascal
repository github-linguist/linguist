var
 a, b: integer;
begin
 a := 10; { binary 1010 }
 b := 12; { binary 1100 }
 writeln('a and b = ', a and b); {  8 = 1000 }
 writeln('a or b  = ', a or b);  { 14 = 1110 }
 writeln('a xor b = ', a xor b)  {  6 = 0110 }
end.
