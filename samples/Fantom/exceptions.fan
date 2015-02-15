// Create a new error class by subclassing sys::Err
const class SpecialErr : Err
{
  // you must provide some message about the error
  // to the parent class, for reporting
  new make () : super ("special error") {}
}

class Main
{
  static Void fn ()
  {
    throw SpecialErr ()
  }

  public static Void main ()
  {
    try
      fn()
    catch (SpecialErr e)
      echo ("Caught " + e)
  }
}
