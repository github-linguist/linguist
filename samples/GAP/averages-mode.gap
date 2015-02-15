mode := function(v)
  local c, m;
  c := Collected(SortedList(v));
  m := Maximum(List(c, x -> x[2]));
  return List(Filtered(c, x -> x[2] = m), y -> y[1]);
end;

mode([ 7, 5, 6, 1, 5, 5, 7, 12, 17, 6, 6, 5, 12, 3, 6 ]);
# [ 5, 6 ]
