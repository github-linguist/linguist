class Main
{
  static Int factorial (Int n)
  {
    Int res := 1
    if (n>1)
      (2..n).each |i| { res *= i }
    return res
  }

  static Int catalanA (Int n)
  {
    return factorial(2*n)/(factorial(n+1) * factorial(n))
  }

  static Int catalanB (Int n)
  {
    if (n == 0)
    {
      return 1
    }
    else
    {
      sum := 0
      n.times |i| { sum += catalanB(i) * catalanB(n-1-i) }
      return sum
    }
  }

  static Int catalanC (Int n)
  {
    if (n == 0)
    {
      return 1
    }
    else
    {
      return catalanC(n-1)*2*(2*n-1)/(n+1)
    }
  }

  public static Void main ()
  {
    (1..15).each |n|
    {
      echo (n.toStr.padl(4) +
            catalanA(n).toStr.padl(10) +
            catalanB(n).toStr.padl(10) +
            catalanC(n).toStr.padl(10))
    }
  }
}
