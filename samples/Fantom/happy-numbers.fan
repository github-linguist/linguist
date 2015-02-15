class Main
{
  static Bool isHappy (Int n)
  {
    Int[] record := [,]
    while (n != 1 && !record.contains(n))
    {
      record.add (n)
      // find sum of squares of digits
      newn := 0
      while (n > 0)
      {
        newn += (n.mod(10) * n.mod(10))
        n = n.div(10)
      }
      n = newn
    }
    return (n == 1)
  }

  public static Void main ()
  {
    i := 1
    count := 0
    while (count < 8)
    {
      if (isHappy (i))
      {
        echo (i)
        count += 1
      }
      i += 1
    }
  }
}
