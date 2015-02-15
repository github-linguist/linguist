import java.lang.reflect.*;

class Example {
    private String _name;
    public Example(String name) { _name = name; }
    public String toString() { return "Hello, I am " + _name; }
}

public class BreakPrivacy {
    public static final void main(String[] args) throws Exception {
        Example foo = new Example("Eric");

        for (Field f : Example.class.getDeclaredFields()) {
	    if (f.getName().equals("_name")) {
                // make it accessible
                f.setAccessible(true);

                // get private field
                System.out.println(f.get(foo));

                // set private field
                f.set(foo, "Edith");
		System.out.println(foo);

                break;
            }
        }
    }
}
