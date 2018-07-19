class Main
{
  static Void displayList (Str[] items)
  {
    items.each |Str item, Int index|
    {
      echo ("$index: $item")
    }
  }

  public static Str getChoice (Str[] items)
  {
    selection := -1
    while (selection == -1)
    {
      displayList (items)
      Env.cur.out.print ("Select: ").flush
      input := Int.fromStr(Env.cur.in.readLine, 10, false)
      if (input != null)
      {
        if (input >= 0 && input < items.size)
        {
          selection = input
        }
      }
      echo ("Try again")
    }
    return items[selection]
  }

  public static Void main ()
  {
    choice := getChoice (["fee fie", "huff and puff", "mirror mirror", "tick tock"])
    echo ("You chose: $choice")
  }
}
