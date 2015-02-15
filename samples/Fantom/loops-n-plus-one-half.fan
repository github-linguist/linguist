class Main
{
  public static Void main ()
  {
    for (Int i := 1; i <= 10; i++)
    {
      Env.cur.out.writeObj (i)
      if (i == 10) break
      Env.cur.out.writeChars (", ")
    }
    Env.cur.out.printLine ("")
  }
}
