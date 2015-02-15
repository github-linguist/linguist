class Main
{
  public static Void main ()
  {
    Int:Str map := [1:"alpha", 2:"beta", 3:"gamma"]

    map.keys.each |Int key|
    {
      echo ("Key is: $key")
    }

    map.vals.each |Str value|
    {
      echo ("Value is: $value")
    }

    map.each |Str value, Int key|
    {
      echo ("Key $key maps to $value")
    }
  }
}
