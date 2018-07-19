class Main
{
  public static Void main ()
  {
    Int lowerLimit := 1
    Int higherLimit := 100

    Int target := (lowerLimit..higherLimit).random
    Int guess
    while (guess != target)
    {
      echo ("Enter a guess: ")
      try
      {
        // read in a line of input, and try to interpret as an Int
        guess = Env.cur.in.readLine.trim.toInt
        if (guess == target)
        {
          echo ("Well guessed!")
        }
        else if (guess < target)
        {
          echo ("Failed - your guess is too small")
        }
        else // if (guess > target)
        {
          echo ("Failed - your guess is too large")
        }
      }
      catch (Err e)
      {
        echo ("Your guess must be an integer")
      }
    }
  }
}
