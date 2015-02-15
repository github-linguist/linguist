class ForBreak
{
  public static Void main ()
  {
    while (true)
    {
      a := Int.random(0..19)
      echo (a)
      if (a == 10) break
      echo (Int.random(0..19))
    }
  }
}
