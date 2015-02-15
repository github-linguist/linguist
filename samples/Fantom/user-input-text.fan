class Main
{
  public static Void main ()
  {
    Env.cur.out.print ("Enter a string: ").flush
    str := Env.cur.in.readLine
    echo ("Entered :$str:")
    Env.cur.out.print ("Enter 75000: ").flush
    Int n
    try n = Env.cur.in.readLine.toInt
    catch (Err e)
    {
      echo ("You had to enter a number")
      return
    }
    echo ("Entered :$n: which is " + ((n == 75000) ? "correct" : "wrong"))
  }
}
