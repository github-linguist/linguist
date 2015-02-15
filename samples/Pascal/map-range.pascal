Program Map(output);

function MapRange(fromRange, toRange: array of real; value: real): real;
  begin
    MapRange := (value-fromRange[0]) * (toRange[1]-toRange[0]) / (fromRange[1]-fromRange[0]) + toRange[0];
  end;

var
  i: integer;
begin
  for i := 0 to 10 do
    writeln (i, ' maps to: ', MapRange([0.0, 10.0], [-1.0, 0.0], i):4:2);
end.
