ackermann = method(m,n,
  cond(
    m zero?, n succ,
    n zero?, ackermann(m pred, 1),
    ackermann(m pred, ackermann(m, n pred)))
)
