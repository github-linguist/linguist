class Compose
{
  static |Obj -> Obj| compose (|Obj -> Obj| fn1, |Obj -> Obj| fn2)
  {
    return |Obj x -> Obj| { fn2 (fn1 (x)) }
  }

  public static Void main ()
  {
    double := |Int x -> Int| { 2 * x }
    |Int -> Int| quad := compose(double, double)
    echo ("Double 3 = ${double(3)}")
    echo ("Quadruple 3 = ${quad (3)}")
  }
}
