program numlist(output);

var
  i: integer;

begin
  for i := 1 to 10 do
    begin
      write(i);
      if i <> 10 then
        write(', ')
    end;
  writeln;
end.
