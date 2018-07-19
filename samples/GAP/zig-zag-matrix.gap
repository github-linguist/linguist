ZigZag := function(n)
  local a, i, j, k;
  a := NullMat(n, n);
  i := 1;
  j := 1;
  for k in [0 .. n*n - 1] do
    a[i][j] := k;
    if (i + j) mod 2 = 0 then
      if j < n then
        j := j + 1;
      else
        i := i + 2;
      fi;
      if i > 1 then
        i := i - 1;
      fi;
    else
      if i < n then
        i := i + 1;
      else
        j := j + 2;
      fi;
      if j > 1 then
        j := j - 1;
      fi;
    fi;
  od;
  return a;
end;

PrintArray(ZigZag(5));
# [ [   0,   1,   5,   6,  14 ],
#   [   2,   4,   7,  13,  15 ],
#   [   3,   8,  12,  16,  21 ],
#   [   9,  11,  17,  20,  22 ],
#   [  10,  18,  19,  23,  24 ] ]
