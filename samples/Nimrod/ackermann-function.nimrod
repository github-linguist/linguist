proc Ackermann(m, n: int64): int64 =
  if m == 0:
    result = n + 1
  elif n == 0:
    result = Ackermann(m - 1, 1)
  else:
    result = Ackermann(m - 1, Ackermann(m, n - 1))
