class Main
{
  public static Void main ()
  {
    str := "Hello,How,Are,You,Today"
    words := str.split(',')
    words.each |Str word|
    {
      echo ("${word}. ")
    }
  }
}
