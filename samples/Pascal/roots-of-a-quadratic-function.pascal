Program QuadraticRoots;

var
  a, b, c, q, f: double;

begin
  a := 1;
  b := -10e9;
  c := 1;
  q := sqrt(a * c) / b;
  f := (1 + sqrt(1 - 4 * q * q)) / 2;

  writeln ('Version 1:');
  writeln ('x1: ', (-b * f / a):16, ', x2: ', (-c / (b * f)):16);

  writeln ('Version 2:');
  q := sqrt(b * b - 4 * a * c);
  if b < 0 then
  begin
    f :=  (-b + q) / 2 * a;
    writeln ('x1: ', f:16, ', x2: ', (c / (a * f)):16);
  end
  else
  begin
    f := (-b - q) / 2 * a;
    writeln ('x1: ', (c / (a * f)):16, ', x2: ', f:16);
  end;
end.
