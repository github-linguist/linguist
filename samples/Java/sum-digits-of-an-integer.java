import java.math.BigInteger;
public class SumDigits {
    public static int sumDigits(long num) {
	return sumDigits(num, 10);
    }
    public static int sumDigits(long num, int base) {
	String s = Long.toString(num, base);
	int result = 0;
	for (int i = 0; i < s.length(); i++)
	    result += Character.digit(s.charAt(i), base);
	return result;
    }
    public static int sumDigits(BigInteger num) {
	return sumDigits(num, 10);
    }
    public static int sumDigits(BigInteger num, int base) {
	String s = num.toString(base);
	int result = 0;
	for (int i = 0; i < s.length(); i++)
	    result += Character.digit(s.charAt(i), base);
	return result;
    }

    public static void main(String[] args) {
	System.out.println(sumDigits(1));
	System.out.println(sumDigits(12345));
	System.out.println(sumDigits(123045));
	System.out.println(sumDigits(0xfe, 16));
	System.out.println(sumDigits(0xf0e, 16));
	System.out.println(sumDigits(new BigInteger("12345678901234567890")));
    }
}
