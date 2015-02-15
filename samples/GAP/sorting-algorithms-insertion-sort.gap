InsertionSort := function(L)
  local n, i, j, x;
  n := Length(L);
  for i in [ 2 .. n ] do
    x := L[i];
    j := i - 1;
    while j >= 1 and L[j] > x do
      L[j + 1] := L[j];
      j := j - 1;
    od;
    L[j + 1] := x;
  od;
end;

s := "BFKRIMPOQACNESWUTXDGLVZHYJ";
InsertionSort(s);
s;
# "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
