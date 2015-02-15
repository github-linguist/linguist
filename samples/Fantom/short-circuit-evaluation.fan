class Main
{
  static Bool a (Bool value)
  {
    echo ("in a")
    return value
  }

  static Bool b (Bool value)
  {
    echo ("in b")
    return value
  }

  public static Void main ()
  {
    [false,true].each |i|
    {
      [false,true].each |j|
      {
        Bool result := a(i) && b(j)
        echo ("a($i) && b($j): " + result)
        result = a(i) || b(j)
        echo ("a($i) || b($j): " + result)
      }
    }
  }
}
