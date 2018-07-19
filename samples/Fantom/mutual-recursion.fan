class Main
{
  static Int f (Int n)
  {
    if (n <= 0) // ensure n > 0
      return 1
    else
      return n - m(f(n-1))
  }

  static Int m (Int n)
  {
    if (n <= 0) // ensure n > 0
      return 0
    else
      return n - f(m(n-1))
  }

  public static Void main ()
  {
    50.times |Int n| { echo (f(n)) }
  }
}
