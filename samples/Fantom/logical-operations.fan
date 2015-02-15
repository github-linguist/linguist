class Main
{
  static Void doOps (Bool arg1, Bool arg2)
  {
    echo ("$arg1 and $arg2 = ${arg1.and(arg2)}")
    echo ("$arg1 or $arg2 = ${arg1.or(arg2)}")
    echo ("not $arg1 = ${arg1.not}")
    echo ("$arg1 xor $arg2 = ${arg1.xor(arg2)}")
  }

  public static Void main ()
  {
    [true,false].each |Bool arg1|
    {
      [true,false].each |Bool arg2|
      {
        doOps (arg1, arg2)
      }
    }
  }
}
