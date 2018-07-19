using concurrent

class Main
{
  public static Void main ()
  {
    echo ("Enter a time to sleep: ")
    input := Env.cur.in.readLine
    try
    {
      time := Duration.fromStr (input)
      echo ("sleeping ...")
      Actor.sleep (time)
      echo ("awake!")
    }
    catch
    {
      echo ("Invalid time entered")
    }
  }
}
