Program CatalanNumbers(output);

function catalanNumber1(n: integer): double;
  begin
    if n = 0 then
      catalanNumber1 := 1.0
    else
      catalanNumber1 := double(4 * n - 2) / double(n + 1) * catalanNumber1(n-1);
  end;

var
  number: integer;

begin
  writeln('Catalan Numbers');
  writeln('Recursion with a fraction:');
  for number := 0 to 14 do
    writeln (number:3, round(catalanNumber1(number)):9);
end.
