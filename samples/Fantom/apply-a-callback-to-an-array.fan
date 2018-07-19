class Main
{
  public static Void main ()
  {
    [1,2,3,4,5].each |Int i| { echo (i) }
    Int[] result := [1,2,3,4,5].map |Int i->Int| { return i * i }
    echo (result)
  }
}
