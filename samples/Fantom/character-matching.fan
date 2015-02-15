class Main
{
  public static Void main ()
  {
    string := "Fantom Language"
    echo ("String is: " + string)
    echo ("does string start with 'Fantom'? " + string.startsWith("Fantom"))
    echo ("does string start with 'Language'? " + string.startsWith("Language"))
    echo ("does string contain 'age'? " + string.contains("age"))
    echo ("does string contain 'page'? " + string.contains("page"))
    echo ("does string end with 'Fantom'? " + string.endsWith("Fantom"))
    echo ("does string end with 'Language'? " + string.endsWith("Language"))

    echo ("Location of 'age' is: " + string.index("age"))
    posn := string.index ("an")
    echo ("First location of 'an' is: " + posn)
    posn = string.index ("an", posn+1)
    echo ("Second location of 'an' is: " + posn)
    posn = string.index ("an", posn+1)
    if (posn == null) echo ("No third location of 'an'")
  }
}
