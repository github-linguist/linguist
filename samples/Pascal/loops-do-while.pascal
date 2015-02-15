program countto6(output);

var
  i: integer;

begin
  i := 0;
  repeat
    i := i + 1;
    writeln(i)
  until i mod 6 = 0
end.
