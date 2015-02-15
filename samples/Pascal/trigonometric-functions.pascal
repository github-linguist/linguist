Program TrigonometricFuntions(output);

uses
  math;

var
  radians, degree: double;

begin
  radians := pi / 4.0;
  degree := 45;
  //  Pascal works in radians.  Necessary degree-radian conversions are shown.
  writeln (sin(radians),'   ', sin(degree/180*pi));
  writeln (cos(radians),'   ', cos(degree/180*pi));
  writeln (tan(radians),'   ', tan(degree/180*pi));
  writeln ();
  writeln (arcsin(sin(radians)),' Rad., or ', arcsin(sin(degree/180*pi))/pi*180,' Deg.');
  writeln (arccos(cos(radians)),' Rad., or ', arccos(cos(degree/180*pi))/pi*180,' Deg.');
  writeln (arctan(tan(radians)),' Rad., or ', arctan(tan(degree/180*pi))/pi*180,' Deg.');
  //  ( radians ) / pi * 180 = deg.
end.
