Program SequenceOfNonSquares(output);

uses
  Math;

var
  m, n, test: longint;

begin
  for n := 1 to 22 do
  begin
    test :=  n + floor(0.5 + sqrt(n));
    write(test, ' ');
  end;
  writeln;

  for n := 1 to 1000000 do
  begin
    test :=  n + floor(0.5 + sqrt(n));
    m := round(sqrt(test));
    if (m*m = test) then
      writeln('square found for n = ', n);
  end;
end.
