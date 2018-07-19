const class U0 : Err
{
  new make () : super ("U0") {}
}

const class U1 : Err
{
  new make () : super ("U1") {}
}

class Main
{
  Int bazCalls := 0

  Void baz ()
  {
    bazCalls += 1
    if (bazCalls == 1)
      throw U0()
    else
      throw U1()
  }

  Void bar ()
  {
    baz ()
  }

  Void foo ()
  {
    2.times
    {
      try
      {
        bar ()
      }
      catch (U0 e)
      {
        echo ("Caught U0")
      }
    }
  }

  public static Void main ()
  {
    Main().foo
  }
}
