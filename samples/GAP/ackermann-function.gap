ack := function(m, n)
  if m = 0 then
    return n + 1;
  elif (m > 0) and (n = 0) then
    return ack(m - 1, 1);
  elif (m > 0) and (n > 0) then
    return ack(m - 1, ack(m, n - 1));
  else
    return fail;
  fi;
end;
