class Main
{
  public static Bool ordered (Str word)
  {
    word.chars.all |Int c, Int i -> Bool|
    {
      (i == (word.size-1) || c <= word.chars[i+1])
    }
  }

  public static Void main ()
  {
    Str[] words := [,]
    File(`unixdict.txt`).eachLine |Str word|
    {
      if (ordered(word))
      {
        if (words.isEmpty || words.first.size < word.size)
        { // reset the list
          words = [word]
        }
        else if (words.size >= 1 && words.first.size == word.size)
        { // add word to existing ones
          words.add (word)
        }
      }
    }
    echo (words.join (" "))
  }
}
