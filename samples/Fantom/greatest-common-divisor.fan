class Main
{
  static Int gcd (Int a, Int b)
  {
    a = a.abs
    b = b.abs
    while (b > 0)
    {
      t := a
      a = b
      b = t % b
    }
    return a
  }

  public static Void main()
  {
    echo ("GCD of 51, 34 is: " + gcd(51, 34))
  }
}
