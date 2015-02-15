interface Thingable {
    String thing();
}

class Delegator {
    public Thingable delegate;

    public String operation() {
        if (delegate == null)
            return "default implementation";
        else
            return delegate.thing();
    }
}

class Delegate implements Thingable {
    public String thing() {
        return "delegate implementation";
    }
}

// Example usage
// Memory management ignored for simplification
public class DelegateExample {
    public static void main(String[] args) {
        // Without a delegate:
        Delegator a = new Delegator();
        assert a.operation().equals("default implementation");

        // With a delegate:
        Delegate d = new Delegate();
        a.delegate = d;
        assert a.operation().equals("delegate implementation");

        // Same as the above, but with an anonymous class:
        a.delegate = new Thingable() {
                public String thing() {
                    return "anonymous delegate implementation";
                }
            };
        assert a.operation().equals("anonymous delegate implementation");
    }
}
