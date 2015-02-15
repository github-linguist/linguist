Program HaversineDemo(output);

uses
  Math;

function haversineDist(th1, ph1, th2, ph2: double): double;
  const
   diameter = 2 * 6372.8;
  var
    dx, dy, dz: double;
  begin
    ph1 := degtorad(ph1 - ph2);
    th1 := degtorad(th1);
    th2 := degtorad(th2);

    dz := sin(th1) - sin(th2);
    dx := cos(ph1) * cos(th1) - cos(th2);
    dy := sin(ph1) * cos(th1);
    haversineDist := arcsin(sqrt(dx**2 + dy**2 + dz**2) / 2) * diameter;
  end;

begin
  writeln ('Haversine distance: ', haversineDist(36.12, -86.67, 33.94, -118.4):7:2, ' km.');
end.
