program compare(input, output);

var
 a, b: integer;

begin
  write('Input an integer number: ');
  readln(a);
  write('Input another integer number: ');
  readln(b);
  if (a < b) then writeln(a, ' is less than ', b);
  if (a = b) then writeln(a, ' is equal to ', b);
  if (a > b) then writeln(a, ' is greater than ', b);
end.
