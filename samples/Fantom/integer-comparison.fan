class Main
{
  public static Void main ()
  {
    try
    {
      Env.cur.out.print ("Enter number 1: ").flush
      num1 := Env.cur.in.readLine.toInt
      Env.cur.out.print ("Enter number 2: ").flush
      num2 := Env.cur.in.readLine.toInt

      if (num1 < num2)
        echo ("$num1 is smaller than $num2")
      else if (num1 == num2)
        echo ("$num1 is equal to $num2")
      else if (num1 > num2)
        echo ("$num1 is greater than $num2")
    }
    catch (Err e)
      echo ("You must enter two integers")
  }
}
