program Factors;
var
  i, number: integer;
begin
  write('Enter a number between 1 and 2147483647: ');
  readln(number);

  for i := 1 to round(sqrt(number)) - 1 do
    if number mod i = 0 then
      write (i, ' ',  number div i, ' ');

  // Check to see if number is a square
  i := round(sqrt(number));
  if i*i = number then
     write(i)
  else if number mod i = 0 then
     write(i, number/i);
  writeln;
end.
