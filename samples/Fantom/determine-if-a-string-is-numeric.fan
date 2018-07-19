class Main
{
  // function to see if str contains a number of any of built-in types
  static Bool readNum (Str str)
  {
    int := Int.fromStr (str, 10, false)  // use base 10
    if (int != null) return true
    float := Float.fromStr (str, false)
    if (float != null) return true
    decimal := Decimal.fromStr (str, false)
    if (decimal != null) return true

    return false
  }

  public static Void main ()
  {
    echo ("For '2': " + readNum ("2"))
    echo ("For '-2': " + readNum ("-2"))
    echo ("For '2.5': " + readNum ("2.5"))
    echo ("For '2a5': " + readNum ("2a5"))
    echo ("For '-2.1e5': " + readNum ("-2.1e5"))
  }
}
