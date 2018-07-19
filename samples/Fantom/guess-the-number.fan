class Main
{
  public static Void main ()
  {
    Str target := (1..10).random.toStr
    Str guess := ""
    while (guess != target)
    {
      echo ("Enter a guess: ")
      guess = Env.cur.in.readLine
      if (guess.trim == target) // 'trim' to remove any spaces/newline
      {
        echo ("Well guessed!")
        break
      }
      else
        echo ("Failed - try again")
    }
  }
}
