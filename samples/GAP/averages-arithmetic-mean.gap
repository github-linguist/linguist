Mean := function(v)
  local n;
  n := Length(v);
  if n = 0 then
    return 0;
  else
    return Sum(v)/n;
  fi;
end;

Mean([3, 1, 4, 1, 5, 9]);
# 23/6
