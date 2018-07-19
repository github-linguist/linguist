class FunctionDefinition
{
  public static Void main ()
  {
    multiply := |Int a, Int b -> Int| { a * b }
    echo ("Multiply 2 and 4: ${multiply(2, 4)}")
  }
}
