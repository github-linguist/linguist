import java.util.InputMismatchException;
import java.util.Scanner;

public class Sleep {
    public static void main(final String[] args) throws InterruptedException {
        try {
            int ms = new Scanner(System.in).nextInt(); //Java's sleep method accepts milliseconds
            System.out.println("Sleeping...");
            Thread.sleep(ms);
            System.out.println("Awake!");
        } catch (InputMismatchException inputMismatchException) {
            System.err.println("Exception: " + inputMismatchException);
        }
    }
}
