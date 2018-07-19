class Main
{
  public static Void main ()
  {
    // create a map which maps Ints to Strs, with given key-value pairs
    Int:Str map := [1:"alpha", 2:"beta", 3:"gamma"]

    // create an empty map
    Map map2 := [:]
    // now add some numbers mapped to their doubles
    10.times |Int i|
    {
      map2[i] = 2*i
    }

  }
}
