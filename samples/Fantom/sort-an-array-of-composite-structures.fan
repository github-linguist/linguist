class Pair // create a composite structure
{
  Str name
  Str value
  new make (Str name, Str value)
  {
    this.name = name
    this.value = value
  }

  override Str toStr ()
  {
    "(Pair: $name, $value)"
  }
}

class Main
{
  public static Void main ()
  {
    // samples
    pairs := [Pair("Fantom", "OO"), Pair("Clojure", "Functional"), Pair("Java", "OO") ]

    sorted := pairs.dup // make a copy of original list
    sorted.sort |Pair a, Pair b -> Int|  // sort using custom comparator
    {
      a.name <=> b.name
    }
    echo ("Started with : " + pairs.join(" "))
    echo ("Finished with: " + sorted.join(" "))
  }
}
