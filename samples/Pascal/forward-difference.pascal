Program ForwardDifferenceDemo(output);

procedure fowardDifference(list: array of integer);
  var
    b: array of integer;
    i, newlength: integer;
  begin
    newlength := length(list) - 1;
    if newlength > 0 then
    begin
      setlength(b, newlength);
      for i := low(b) to high(b) do
      begin
        b[i] := list[i+1] - list[i];
        write (b[i]:6);
      end;
      writeln;
      fowardDifference(b);
    end;
  end;

var
  a: array [1..10] of integer = (90, 47, 58, 29, 22, 32, 55, 5, 55, 73);
begin
  fowardDifference(a);
end.
