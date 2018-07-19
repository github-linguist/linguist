ackermann = (m, n) ->
  if m is 0 then n + 1
  else if m > 0 and n is 0 then ackermann m - 1, 1
  else ackermann m - 1, ackermann m, n - 1
