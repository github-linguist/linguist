class SumSquares
{
  static Int sumSquares (Int[] numbers)
  {
    Int sum := 0
    numbers.each |n| { sum += n * n }
    return sum
  }

  public static Void main ()
  {
    Int[] n := [,]
    echo ("Sum of squares of $n = ${sumSquares(n)}")
    n = [1,2,3,4,5]
    echo ("Sum of squares of $n = ${sumSquares(n)}")
  }
}
