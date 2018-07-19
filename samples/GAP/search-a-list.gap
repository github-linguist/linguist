# First position is built-in
haystack := Eratosthenes(10000);;
needle := 8999;;
Position(haystack, needle);
# 1117

LastPosition := function(L, x)
  local old, new;
  old := 0;
  new := 0;
  while new <> fail do
    new := Position(L, x, old);
    if new <> fail then
      old := new;
    fi;
  od;
  return old;
end;

a := Shuffle(List([1 .. 100], x -> x mod 10));
# [ 0, 2, 4, 5, 3, 1, 0, 4, 8, 8, 2, 7, 6, 3, 3, 6, 4, 4, 3, 0, 7, 1, 8, 7, 2, 4, 7, 9, 4, 9, 4, 5, 9, 9, 6, 7, 8, 2, 3,
#   5, 1, 5, 4, 2, 0, 9, 6, 1, 1, 2, 2, 0, 5, 7, 6, 8, 8, 3, 1, 9, 5, 1, 9, 6, 8, 9, 2, 0, 6, 2, 1, 6, 1, 1, 2, 5, 3, 3,
#   0, 3, 5, 7, 5, 4, 6, 8, 0, 9, 8, 3, 7, 8, 0, 4, 9, 7, 0, 6, 5, 7 ]
Position(a, 0);
# 1
LastPosition(a, 0);
# 97
