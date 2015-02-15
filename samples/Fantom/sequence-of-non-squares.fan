class Main
{
  static Float fn (Int n)
  {
    n + (0.5f + (n * 1.0f).sqrt).floor
  }

  static Bool isSquare (Float n)
  {
    n.sqrt.floor == n.sqrt
  }

  public static Void main ()
  {
    (1..22).each |n|
    {
      echo ("$n is ${fn(n)}")
    }
    echo ((1..1000000).toList.any |n| { isSquare (fn(n)) } )
  }
}
