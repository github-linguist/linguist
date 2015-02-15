Median := function(v)
  local n, w;
  w := SortedList(v);
  n := Length(v);
  return (w[QuoInt(n + 1, 2)] + w[QuoInt(n, 2) + 1]) / 2;
end;

a := [41, 56, 72, 17, 93, 44, 32];
b := [41, 72, 17, 93, 44, 32];

Median(a);
# 44
Median(b);
# 85/2
