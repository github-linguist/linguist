Program WriteNumbers;

const
  x: array [1..4] of double = (1, 2, 3, 1e11);
  xprecision = 3;
  yprecision = 5;
  baseDigits = 7;

var
  i: integer;
  filename: text;

begin
  assign (filename, 'filename');
  rewrite (filename);
  for i := 1 to 4 do
    writeln (filename, x[i]:baseDigits+xprecision, sqrt(x[i]):baseDigits+yprecision);
  close (filename);
end.
