Find := function(v, x)
  local low, high, mid;
  low := 1;
  high := Length(v);
  while low <= high do
    mid := QuoInt(low + high, 2);
    if v[mid] > x then
      high := mid - 1;
    elif v[mid] < x then
      low := mid + 1;
    else
      return mid;
    fi;
  od;
  return fail;
end;

u := [1..10]*7;
# [ 7, 14, 21, 28, 35, 42, 49, 56, 63, 70 ]
Find(u, 34);
# fail
Find(u, 35);
# 5
