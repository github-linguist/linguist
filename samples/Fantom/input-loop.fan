class Main
{
  public static Void main ()
  {
    // example of reading by line
    str := "first\nsecond\nthird\nword"
    inputStream := str.in

    inputStream.eachLine |Str line|
    {
      echo ("Line is: $line")
    }

    // example of reading by word
    str = "first second third word"
    inputStream = str.in

    word := inputStream.readStrToken // reads up to but excluding next space
    while (word != null)
    {
      echo ("Word: $word")
      inputStream.readChar // skip over the preceding space!
      word = inputStream.readStrToken
    }
  }
}
