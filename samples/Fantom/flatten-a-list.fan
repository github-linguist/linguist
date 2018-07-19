class Main
{
  // uses recursion to flatten a list
  static List myflatten (List items)
  {
    List result := [,]
    items.each |item|
    {
      if (item is List)
        result.addAll (myflatten(item))
      else
        result.add (item)
    }
    return result
  }

  public static Void main ()
  {
    List sample := [[1], 2, [[3,4], 5], [[[,]]], [[[6]]], 7, 8, [,]]
    // there is a built-in flatten method for lists
    echo ("Flattened list 1: " + sample.flatten)
    // or use the function 'myflatten'
    echo ("Flattened list 2: " + myflatten (sample))
  }
}
