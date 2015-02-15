import java.security.SecureRandom;

public class RandomExample {
  public static void main(String[] args) {
    SecureRandom rng = new SecureRandom();

    /* Prints a random signed 32-bit integer. */
    System.out.println(rng.nextInt());
  }
}
