CollatzSequence := function(n)
  local v;
  v := [ n ];
  while n > 1 do
    if IsEvenInt(n) then
      n := QuoInt(n, 2);
    else
      n := 3*n + 1;
    fi;
    Add(v, n);
  od;
  return v;
end;

CollatzLength := function(n)
  local m;
  m := 1;
  while n > 1 do
    if IsEvenInt(n) then
      n := QuoInt(n, 2);
    else
      n := 3*n + 1;
    fi;
    m := m + 1;
  od;
  return m;
end;

CollatzMax := function(a, b)
  local n, len, nmax, lmax;
  lmax := 0;
  for n in [a .. b] do
    len := CollatzLength(n);
    if len > lmax then
      nmax := n;
      lmax := len;
    fi;
  od;
  return [ nmax, lmax ];
end;

CollatzSequence(27);
# [ 27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206,
#   103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502,
#   251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429,
#   7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300,
#   650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1 ]
CollatzLength(27);
# 112

CollatzMax(1, 100);
# [ 97, 119 ]
CollatzMax(1, 1000);
# [ 871, 179 ]
CollatzMax(1, 10000);
# [ 6171, 262 ]
CollatzMax(1, 100000);
# [ 77031, 351 ]
CollatzMax(1, 1000000);
# [ 837799, 525 ]
