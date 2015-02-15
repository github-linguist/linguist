class A
{
  public Void doit (Int n)
  {
    echo ("known function called on $n")
  }

  // override the 'trap' method, which catches dynamic invocations of methods
  override Obj? trap(Str name, Obj?[]? args := null)
  {
    try
    {
      return super.trap(name, args)
    }
    catch (UnknownSlotErr err)
    {
      echo ("In trap, you called: " + name + " with args " + args.join(","))
      return null
    }
  }
}

class Main
{
  public static Void main ()
  {
    a := A()
    // note the dynamic dispatch
    a->doit (1)
    a->methodName (1, 2, 3)
  }
}
