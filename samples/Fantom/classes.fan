class MyClass
{
  // an instance variable
  Int x

  // a constructor, providing default value for instance variable
  new make (Int x := 1)
  {
    this.x = x
  }

  // a method, return double the number x
  public Int double ()
  {
    return 2 * x
  }
}

class Main
{
  public static Void main ()
  {
    a := MyClass (2)  // instantiates the class, with x = 2
    b := MyClass()    // instantiates the class, x defaults to 1
    c := MyClass { x = 3 }  // instantiates the class, sets x to 3
  }
}
