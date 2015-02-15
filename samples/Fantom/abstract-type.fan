abstract class X
{
  Void method1 ()
  {
    echo ("Method 1 in X")
  }

  abstract Void method2 ()
}

class Y : X
{
  // Y must override the abstract method in X
  override Void method2 ()
  {
    echo ("Method 2 in Y")
  }
}

class Main
{
  public static Void main ()
  {
    y := Y()
    y.method1
    y.method2
  }
}
