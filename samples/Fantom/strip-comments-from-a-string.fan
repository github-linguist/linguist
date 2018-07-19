class Main
{
  static Str removeComment (Str str)
  {
    regex := Regex <|(;|#)|>
    matcher := regex.matcher (str)
    if (matcher.find)
      return str[0..<matcher.start]
    else
      return str
  }

  public static Void main ()
  {
    echo (removeComment ("String with comment here"))
    echo (removeComment ("String with comment # here"))
    echo (removeComment ("String with comment ; here"))
  }
}
