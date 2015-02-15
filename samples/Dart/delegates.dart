class Delegator {
  var delegate;

  String operation() {
    if (delegate == null)
      return "default implementation";
    else
      return delegate.thing();
  }
}

class Delegate {
  String thing() => "delegate implementation";
}

main() {
  // Without a delegate:
  Delegator a = new Delegator();
  Expect.equals("default implementation",a.operation());

  // any object doesn't work unless we can check for existing methods
  // a.delegate=new Object();
  // Expect.equals("default implementation",a.operation());

  // With a delegate:
  Delegate d = new Delegate();
  a.delegate = d;
  Expect.equals("delegate implementation",a.operation());
}
