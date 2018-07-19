fun ackermann
  {m,n:nat} .<m,n>.
  (m: int m, n: int n): Nat =
  case+ (m, n) of
  | (0, _) => n+1
  | (_, 0) =>> ackermann (m-1, 1)
  | (_, _) =>> ackermann (m-1, ackermann (m, n-1))
// end of [ackermann]
