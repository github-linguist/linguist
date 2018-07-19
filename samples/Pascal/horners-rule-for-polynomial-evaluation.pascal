Program HornerDemo(output);

function horner(a: array of double; x: double): double;
  var
    i: integer;
  begin
    horner := a[high(a)];
    for i := high(a) - 1 downto low(a) do
      horner := horner * x + a[i];
  end;

const
  poly: array [1..4] of double = (-19.0, 7.0, -4.0, 6.0);

begin
  write ('Horner calculated polynomial of 6*x^3 - 4*x^2 + 7*x - 19 for x = 3: ');
  writeln (horner (poly, 3.0):8:4);
end.
