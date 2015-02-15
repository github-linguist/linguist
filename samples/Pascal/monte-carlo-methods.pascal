Program MonteCarlo(output);

uses
  Math;

function MC_Pi(expo: integer): real;
  var
    x, y: real;
    i, hits, samples: longint;
  begin
    samples := 10**expo;
    hits := 0;
    randomize;
    for i := 1 to samples do
    begin
      x := random;
      y := random;
      if sqrt(x*x + y*y) < 1.0 then
        inc(hits);
    end;
    MC_Pi := 4.0 * hits / samples;
  end;

var
  i: integer;
begin
  for i := 4 to 8 do
    writeln (10**i, ' samples give ', MC_Pi(i):7:5, ' as pi.');
end.
