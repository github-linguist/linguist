class Main
{
  public static Void main ()
  {
    keys := [1,2,3,4,5]
    values := ["one", "two", "three", "four", "five"]

    // create an empty map
    map := [:]
    // add the key-value pairs to it
    keys.size.times |Int index|
    {
      map.add(keys[index], values[index])
    }
  }
}
