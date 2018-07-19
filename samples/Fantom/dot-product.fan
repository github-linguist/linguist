class DotProduct
{
  static Int dotProduct (Int[] a, Int[] b)
  {
    Int result := 0
    [a.size,b.size].min.times |i|
    {
      result += a[i] * b[i]
    }
    return result
  }

  public static Void main ()
  {
    Int[] x := [1,2,3,4]
    Int[] y := [2,3,4]

    echo ("Dot product of $x and $y is ${dotProduct(x, y)}")
  }
}
