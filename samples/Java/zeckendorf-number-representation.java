import java.util.*;

class Zeckendorf
{
  public static String getZeckendorf(int n)
  {
    if (n == 0)
      return "0";
    List<Integer> fibNumbers = new ArrayList<Integer>();
    fibNumbers.add(1);
    int nextFib = 2;
    while (nextFib <= n)
    {
      fibNumbers.add(nextFib);
      nextFib += fibNumbers.get(fibNumbers.size() - 2);
    }
    StringBuilder sb = new StringBuilder();
    for (int i = fibNumbers.size() - 1; i >= 0; i--)
    {
      int fibNumber = fibNumbers.get(i);
      sb.append((fibNumber <= n) ? "1" : "0");
      if (fibNumber <= n)
        n -= fibNumber;
    }
    return sb.toString();
  }

  public static void main(String[] args)
  {
    for (int i = 0; i <= 20; i++)
      System.out.println("Z(" + i + ")=" + getZeckendorf(i));
  }
}
