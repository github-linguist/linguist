Program PascalsTriangle(output);

procedure Pascal(r : Integer);
  var
    i, c, k : Integer;
  begin
    for i := 0 to r-1 do
    begin
      c := 1;
      for k := 0 to i do
      begin
        write(c:3);
        c := (c * (i-k)) div (k+1);
      end;
      writeln;
   end;
end;

begin
  Pascal(9)
end.
