# return a list of two lists :
# first is the list of months with five weekends between years y1 and y2 (included)
# second is the list of years without such months, in the same interval
FiveWeekends := function(y1, y2)
  local L, yL, badL, d, m, y;
  L := [ ];
  badL := [ ];
  for y in [y1 .. y2] do
    yL := [ ];
    for m in [1, 3, 5, 7, 8, 10, 12] do
      if WeekDay([1, m, y]) = "Fri" then
        d := StringDate([1, m, y]);
        Add(yL, d{[4 .. 11]});
      fi;
    od;
    if Length(yL) = 0 then
      Add(badL, y);
    else
      Append(L, yL);
    fi;
  od;
  return [ L, badL ];
end;

r := FiveWeekends(1900, 2100);;
n := Length(r[1]);
# 201
Length(r[2]);
# 29
r[1]{[1 .. 5]};
# [ "Mar-1901", "Aug-1902", "May-1903", "Jan-1904", "Jul-1904" ]
r[1]{[n-4 .. n]};
# [ "Mar-2097", "Aug-2098", "May-2099", "Jan-2100", "Oct-2100" ]
