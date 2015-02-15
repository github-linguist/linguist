class Fibonacci
{
  public static int[] lucas(int n, int numRequested)
  {
    if (n < 2)
      throw new IllegalArgumentException("Fibonacci value must be at least 2");
    return fibonacci((n == 2) ? new int[] { 2, 1 } : lucas(n - 1, n), numRequested);
  }

  public static int[] fibonacci(int n, int numRequested)
  {
    if (n < 2)
      throw new IllegalArgumentException("Fibonacci value must be at least 2");
    return fibonacci((n == 2) ? new int[] { 1, 1 } : fibonacci(n - 1, n), numRequested);
  }

  public static int[] fibonacci(int[] startingValues, int numRequested)
  {
    int[] output = new int[numRequested];
    int n = startingValues.length;
    System.arraycopy(startingValues, 0, output, 0, n);
    for (int i = n; i < numRequested; i++)
      for (int j = 1; j <= n; j++)
        output[i] += output[i - j];
    return output;
  }

  public static void main(String[] args)
  {
    for (int n = 2; n <= 10; n++)
    {
      System.out.print("nacci(" + n + "):");
      for (int value : fibonacci(n, 15))
        System.out.print(" " + value);
      System.out.println();
    }
    for (int n = 2; n <= 10; n++)
    {
      System.out.print("lucas(" + n + "):");
      for (int value : lucas(n, 15))
        System.out.print(" " + value);
      System.out.println();
    }
  }
}
