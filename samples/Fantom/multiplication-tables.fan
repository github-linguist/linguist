class Main
{
  static Void multiplicationTable (Int n)
  {
    // print column headings
    echo ("    |" + (1..n).map |Int a -> Str| { a.toStr.padl(4)}.join("") )
    echo ("-----" + (1..n).map { "----" }.join("") )
    // work through each row
    (1..n).each |i|
    {
      echo ( i.toStr.padl(4) + "|" +
             Str.spaces(4*(i-1)) +
             (i..n).map |Int j -> Str| { (i*j).toStr.padl(4)}.join("") )
    }
  }

  public static Void main ()
  {
    multiplicationTable (12)
  }
}
