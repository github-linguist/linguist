Program Roots;

var
  root: record  // poor man's complex type.
    r: real;
    i: real;
  end;
  i, n:  integer;
  angle: real;

begin
  for n := 2 to 7 do
  begin
    angle := 0.0;
    write(n, ': ');
    for i := 1 to n do
    begin
      root.r := cos(angle);
      root.i := sin(angle);
      write(root.r:8:5, root.i:8:5, 'i ');
      angle := angle + (2.0 * pi / n);
    end;
    writeln;
  end;
end.
