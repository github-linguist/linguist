class AccumulatorFactory
{
  static |Num -> Num| accumulator (Num sum)
  {
    return |Num a -> Num|
    { // switch on type of sum
      if (sum is Int)
      { // and then type of a
        if (a is Int)
          return sum = sum->plus(a)
        else if (a is Float)
          return sum = sum->plusFloat(a)
        else
          return sum = sum->plusDecimal(a)
      }
      else if (sum is Float)
      {
        if (a is Int)
          return sum = sum->plusInt(a)
        else if (a is Float)
          return sum = sum->plus(a)
        else
          return sum = sum->plusDecimal(a)
      }
      else // if (sum is Decimal)
      {
        if (a is Int)
          return sum = sum->plusInt(a)
        else if (a is Float)
          return sum = sum->plusFloat(a)
        else
          return sum = sum->plus(a)
      }
    }
  }

  public static Void main ()
  {
    x := accumulator (3.1)
    y := accumulator (3f)
    echo (x(5))              // the Decimal sum combines with an Int
    echo (x(2))
    echo (y(5.1))            // the Float sum combines with a Decimal

    x = accumulator (1)
    x (5)
    accumulator (3)
    echo (x(2.3))          // the Int sum is now a Decimal
  }
}
