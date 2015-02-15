ZapGremlins := function(s)
  local upper, lower, c, i, n, t;
  upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  lower := "abcdefghijklmnopqrstuvwxyz";
  t := [ ];
  i := 1;
  for c in s do
    n := Position(upper, c);
    if n <> fail then
      t[i] := lower[n];
      i := i + 1;
    else
      n := Position(lower, c);
      if n <> fail then
        t[i] := c;
        i := i + 1;
      fi;
    fi;
  od;
  return t;
end;

IsPalindrome := function(s)
  local t;
  t := ZapGremlins(s);
  return t = Reversed(t);
end;
