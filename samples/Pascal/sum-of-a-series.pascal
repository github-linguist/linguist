Program SumSeries;

var
  S: double;
  i: integer;

function f(number: integer): double;
begin
  f := 1/(number*number);
end;

begin
  S := 0;
  for i := 1 to 1000 do
    S := S + f(i);
  writeln('The sum of 1/x^2 from 1 to 1000 is: ', S:10:8);
  writeln('Whereas pi^2/6 is:                  ', pi*pi/6:10:8);
end.
