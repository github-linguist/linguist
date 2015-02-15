program BoxTheCompass(output);

function compasspoint(angle: real): string;
  const
    points: array [1..32] of string =
      ('North             ', 'North by east     ', 'North-northeast   ', 'Northeast by north',
       'Northeast         ', 'Northeast by east ', 'East-northeast    ', 'East by north     ',
       'East              ', 'East by south     ', 'East-southeast    ', 'Southeast by east ',
       'Southeast         ', 'Southeast by south', 'South-southeast   ', 'South by east     ',
       'South             ', 'South by west     ', 'South-southwest   ', 'Southwest by south',
       'Southwest         ', 'Southwest by west ', 'West-southwest    ', 'West by south     ',
       'West              ', 'West by north     ', 'West-northwest    ', 'Northwest by west ',
       'Northwest         ', 'Northwest by north', 'North-northwest   ', 'North by west     '
      );
  var
    index: integer;
  begin
    index := round (angle / 11.25);
    index := index mod 32 + 1;
    compasspoint := points[index];
  end;

var
  i:       integer;
  heading: real;

begin
  for i := 0 to 32 do
  begin
    heading := i * 11.25;
    case (i mod 3) of
      1: heading := heading + 5.62;
      2: heading := heading - 5.62;
    end;
    writeln((i mod 32) + 1:2, ' ', compasspoint(heading), ' ', heading:8:4);
  end;
end.
