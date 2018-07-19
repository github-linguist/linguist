# Spiral matrix with numbers 1 .. n<sup>2</sup>, more natural in GAP
SpiralMatrix := function(n)
  local i, j, k, di, dj, p, vi, vj, imin, imax, jmin, jmax;
  a := NullMat(n, n);
  vi := [ 1, 0, -1, 0 ];
  vj := [ 0, 1, 0, -1 ];
  imin := 0;
  imax := n;
  jmin := 1;
  jmax := n + 1;
  p := 1;
  di := vi[p];
  dj := vj[p];
  i := 1;
  j := 1;
  for k in [1 .. n*n] do
    a[j][i] := k;
    i := i + di;
    j := j + dj;
    if i < imin or i > imax or j < jmin or j > jmax then
      i := i - di;
      j := j - dj;
      p := RemInt(p, 4) + 1;
      di := vi[p];
      dj := vj[p];
      i := i + di;
      j := j + dj;
      if p = 1 then
        imax := imax - 1;
      elif p = 2 then
        jmax := jmax - 1;
      elif p = 3 then
        imin := imin + 1;
      else
        jmin := jmin + 1;
      fi;
    fi;
  od;
  return a;
end;

PrintArray(SpiralMatrix(5));
# [ [   1,   2,   3,   4,   5 ],
#   [  16,  17,  18,  19,   6 ],
#   [  15,  24,  25,  20,   7 ],
#   [  14,  23,  22,  21,   8 ],
#   [  13,  12,  11,  10,   9 ] ]
