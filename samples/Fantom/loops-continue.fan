class LoopsContinue
{
  public static Void main ()
  {
    for (Int i := 1; i <= 10; ++i)
    {
      Env.cur.out.print (i)
      if (i % 5 == 0)
      {
        Env.cur.out.printLine ("")
        continue
      }
      Env.cur.out.print (", ")
    }
    Env.cur.out.printLine ("")
  }
}
