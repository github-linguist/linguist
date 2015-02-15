import java.lang.reflect.Method;

class Example {
  public int foo(int x) {
    return 42 + x;
  }
}

public class Main {
  public static void main(String[] args) throws Exception {
    Object example = new Example();
    String name = "foo";
    Class<?> clazz = example.getClass();
    Method meth = clazz.getMethod(name, int.class);
    Object result = meth.invoke(example, 5); // result is int wrapped in an object (Integer)
    System.out.println(result);        // prints "47"
  }
}
