class Main
{
  static Int fib (Int n)
  {
    if (n < 2) return n
    fibNums := [1, 0]
    while (fibNums.size <= n)
    {
      fibNums.insert (0, fibNums[0] + fibNums[1])
    }
    return fibNums.first
  }

  public static Void main ()
  {
    20.times |n|
    {
      echo ("Fib($n) is ${fib(n)}")
    }
  }
}
