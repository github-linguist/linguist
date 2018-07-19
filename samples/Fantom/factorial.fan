class Main
{
  static Int factorialRecursive (Int n)
  {
    if (n <= 1)
      return 1
    else
      return n * (factorialRecursive (n - 1))
  }

  static Int factorialIterative (Int n)
  {
    Int product := 1
    for (Int i := 2; i <=n ; ++i)
    {
      product *= i
    }
    return product
  }

  static Int factorialFunctional (Int n)
  {
    (1..n).toList.reduce(1) |a,v|
    {
      v->mult(a) // use a dynamic invoke
      // alternatively, cast a:  v * (Int)a
    }
  }

  public static Void main ()
  {
    echo (factorialRecursive(20))
    echo (factorialIterative(20))
    echo (factorialFunctional(20))
  }
}
