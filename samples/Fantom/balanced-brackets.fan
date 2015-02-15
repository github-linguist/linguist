class Main
{
  static Bool matchingBrackets (Str[] brackets)
  {
    Int opened := 0
    Int i := 0
    while (i < brackets.size)
    {
      if (brackets[i] == "[")
        opened += 1
      else
        opened -= 1
      if (opened < 0) return false
      i += 1
    }
    return true
  }

  public static Void main (Str[] args)
  {
    if (args.size == 1 && Int.fromStr(args[0], 10, false) != null)
    {
      n := Int.fromStr(args[0])
      Str[] brackets := [,]
      20.times
      {
        brackets = [,]
        // create a random set of brackets
        n.times { brackets.addAll (["[", "]"]) }
        n.times { brackets.swap(Int.random(0..<2*n), Int.random(0..<2*n)) }
        // report if matching or not
        if (matchingBrackets(brackets))
          echo (brackets.join(" ") + " Matching")
        else
          echo (brackets.join(" ") + " not matching")
      }
    }
  }
}
