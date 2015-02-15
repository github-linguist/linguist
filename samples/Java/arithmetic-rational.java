class BigRationalFindPerfectNumbers {
  public static void main(String[] args) {
    System.out.println("Running BigRational built-in tests");
    if (BigRational.testFeatures()) {
      int MAX_NUM = (1 << 19);
      System.out.println();
      System.out.println("Searching for perfect numbers in the range [1, " + (MAX_NUM - 1) + "]");
      BigRational TWO = BigRational.valueOf(2);
      for (int i = 1; i < MAX_NUM; i++) {
        BigRational reciprocalSum = BigRational.ONE;
        if (i > 1)
          reciprocalSum = reciprocalSum.add(BigRational.valueOf(i).reciprocal());
        int maxDivisor = (int)Math.sqrt(i);
        if (maxDivisor >= i)
          maxDivisor--;
        for (int divisor = 2; divisor <= maxDivisor; divisor++) {
          if ((i % divisor) == 0) {
            reciprocalSum = reciprocalSum.add(BigRational.valueOf(divisor).reciprocal());
            int dividend = i / divisor;
            if (divisor != dividend)
              reciprocalSum = reciprocalSum.add(BigRational.valueOf(dividend).reciprocal());
          }
        }
        if (reciprocalSum.equals(TWO))
          System.out.println(String.valueOf(i) + " is a perfect number");
      }
    }
    return;
  }
}
