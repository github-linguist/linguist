public class MiddleThreeDigits {

    public static void main(String[] args) {
        final long[] passing = {123, 12345, 1234567, 987654321, 10001, -10001,
            -123, -100, 100, -12345, Long.MIN_VALUE, Long.MAX_VALUE};

        final int[] failing = {1, 2, -1, -10, 2002, -2002, 0, Integer.MIN_VALUE,
            Integer.MAX_VALUE};

        for (long n : passing)
            System.out.printf("middleThreeDigits(%s): %s\n", n, middleThreeDigits(n));

        for (int n : failing)
            System.out.printf("middleThreeDigits(%s): %s\n", n, middleThreeDigits(n));
    }

    public static <T> String middleThreeDigits(T n) {
        String s = String.valueOf(n);
        if (s.charAt(0) == '-')
            s = s.substring(1);
        int len = s.length();
        if (len < 3 || len % 2 == 0)
            return "Need odd and >= 3 digits";
        int mid = len / 2;
        return s.substring(mid - 1, mid + 2);
    }
}
