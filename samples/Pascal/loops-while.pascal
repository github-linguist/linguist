program divby2(output);

var
  i: integer;

begin
  i := 1024;
  while i > 0 do
    begin
      writeln(i);
      i := i div 2
    end
end.
