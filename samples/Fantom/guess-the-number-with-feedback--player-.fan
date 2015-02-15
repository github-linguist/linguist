class Main
{
  public static Void main ()
  {
    Int lowerLimit := 1
    Int higherLimit := 100

    echo ("Think of a number between 1 and 100")
    echo ("Press 'enter' when ready")
    Env.cur.in.readLine

    while (true)
    {
      if (higherLimit < lowerLimit)
      { // check that player is not cheating!
        echo ("Something has gone wrong ... I give up")
        break
      }
      myGuess := (higherLimit + lowerLimit) / 2
      echo ("My guess is $myGuess")
      echo ("Enter 'H' if your number is higher, 'L' if lower, or 'E' if equal")
      switch (Env.cur.in.readLine.trim.upper)
      {
        case "E":
          echo ("I got it correct - thankyou!")
          break // game over
        case "H":
          lowerLimit = myGuess + 1
        case "L":
          higherLimit = myGuess - 1
        default:
          echo ("Pardon? Let's try that again")
      }
    }
  }
}
