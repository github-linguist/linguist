class U0 extends Exception { }
class U1 extends Exception { }

public class ExceptionsTest {
    public static void foo() throws U1 {
        for (int i = 0; i <= 1; i++) {
            try {
                bar(i);
            } catch (U0 e) {
                System.out.println("Function foo caught exception U0");
            }
        }
    }

    public static void bar(int i) throws U0, U1 {
        baz(i); // Nest those calls
    }

    public static void baz(int i) throws U0, U1 {
        if (i == 0)
            throw new U0();
        else
            throw new U1();
    }

    public static void main(String[] args) throws U1 {
        foo();
    }
}
