class Main
{
  public static Void main ()
  {
    // sample strings from Lisp example
    strs := ["Cat", "apple", "Adam", "zero", "Xmas", "quit",
             "Level", "add", "Actor", "base", "butter"]

    sorted := strs.dup // make a copy of original list
    sorted.sort |Str a, Str b -> Int|  // sort using custom comparator
    {
      if (b.size == a.size)           // if size is same
        return a.compareIgnoreCase(b) // then sort in ascending lexicographic order, ignoring case
      else
        return b.size <=> a.size      // else sort in descending size order
    }
    echo ("Started with : " + strs.join(" "))
    echo ("Finished with: " + sorted.join(" "))
  }
}
