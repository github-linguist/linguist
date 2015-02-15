import java.math.BigInteger;

public class MillerRabinPrimalityTest {
  public static void main(String[] args) {
    BigInteger n = new BigInteger(args[0]);
    int certainty = Integer.parseInt(args[1]);
    System.out.println(n.toString() + " is " + (n.isProbablePrime(certainty) ? "probably prime" : "composite"));
  }
}
