import java.math.BigInteger;

class Program {
    public static void main(String[] args) {
        BigInteger x = BigInteger.valueOf(5).pow(BigInteger.valueOf(4).pow(BigInteger.valueOf(3).pow(2).intValue()).intValue());
        String y = x.toString();
        int l = y.length();
        System.out.printf("5**4**3**2 = %s...%s and has %d digits\n",
                          y.substring(0,20), y.substring(l-20), l);
    }
}
