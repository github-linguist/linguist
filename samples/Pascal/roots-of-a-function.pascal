Program RootsFunction;

var
  e, x, step, value: double;
  s: boolean;
  i, limit: integer;
  x1, x2, d: double;

function f(const x: double): double;
  begin
    f := x*x*x - 3*x*x + 2*x;
  end;

begin
  x    := -1;
  step := 1.0e-6;
  e    := 1.0e-9;
  s    := (f(x) > 0);

  writeln('Version 1: simply stepping x:');
  while x < 3.0 do
  begin
    value := f(x);
    if abs(value) < e then
    begin
      writeln ('root found at x = ', x);
      s := not s;
    end
    else if ((value > 0) <> s) then
    begin
      writeln ('root found at x = ', x);
      s := not s;
    end;
    x := x + step;
  end;

  writeln('Version 2: secant method:');
  x1 := -1.0;
  x2 :=  3.0;
  e  :=  1.0e-15;
  i  :=  1;
  limit := 300;
  while true do
  begin
    if i > limit then
    begin
      writeln('Error: function not converging');
      exit;
    end;
    d := (x2 - x1) / (f(x2) - f(x1)) * f(x2);
    if abs(d) < e then
    begin
      if d = 0 then
        write('Exact ')
      else
        write('Approximate ');
      writeln('root found at x = ', x2);
      exit;
    end;
    x1 := x2;
    x2 := x2 - d;
    i  := i + 1;
  end;
end.
