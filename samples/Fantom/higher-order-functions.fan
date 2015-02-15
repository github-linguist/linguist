class Main
{
  // apply given function to two arguments
  static Int performOp (Int arg1, Int arg2, |Int, Int -> Int| fn)
  {
    fn (arg1, arg2)
  }

  public static Void main ()
  {
    echo (performOp (2, 5, |Int a, Int b -> Int| { a + b }))
    echo (performOp (2, 5, |Int a, Int b -> Int| { a * b }))
  }
}
