doors := function(n)
  local a,j,s;
  a := [ ];
  for j in [1 .. n] do
    a[j] := 0;
  od;
  for s in [1 .. n] do
    j := s;
    while j <= n do
      a[j] := 1 - a[j];
      j := j + s;
    od;
  od;
  return Filtered([1 .. n], j -> a[j] = 1);
end;

doors(100);
# [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 ]
