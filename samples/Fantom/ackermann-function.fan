class Main
{
  // assuming m,n are positive
  static Int ackermann (Int m, Int n)
  {
    if (m == 0)
      return n + 1
    else if (n == 0)
      return ackermann (m - 1, 1)
    else
      return ackermann (m - 1, ackermann (m, n - 1))
  }

  public static Void main ()
  {
    (0..3).each |m|
    {
      (0..6).each |n|
      {
        echo ("Ackerman($m, $n) = ${ackermann(m, n)}")
      }
    }
  }
}
