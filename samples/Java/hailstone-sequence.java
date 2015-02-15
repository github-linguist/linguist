import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Hailstone {

  public static List<Long> getHailstoneSequence(long n) {
    if (n <= 0)
      throw new IllegalArgumentException("Invalid starting sequence number");
    List<Long> list = new ArrayList<Long>();
    list.add(Long.valueOf(n));
    while (n != 1) {
      if ((n & 1) == 0)
        n = n / 2;
      else
        n = 3 * n + 1;
      list.add(Long.valueOf(n));
    }
    return list;
  }

  public static void main(String[] args) {
    List<Long> sequence27 = getHailstoneSequence(27);
    System.out.println("Sequence for 27 has " + sequence27.size() + " elements: " + sequence27);

    long MAX = 100000;
    // Simple way
    {
      long highestNumber = 1;
      int highestCount = 1;
      for (long i = 2; i < MAX; i++) {
        int count = getHailstoneSequence(i).size();
        if (count > highestCount) {
          highestCount = count;
          highestNumber = i;
        }
      }
      System.out.println("Method 1, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }

    // More memory efficient way
    {
      long highestNumber = 1;
      int highestCount = 1;
      for (long i = 2; i < MAX; i++) {
        int count = 1;
        long n = i;
        while (n != 1) {
          if ((n & 1) == 0)
            n = n / 2;
          else
            n = 3 * n + 1;
          count++;
        }
        if (count > highestCount) {
          highestCount = count;
          highestNumber = i;
        }
      }
      System.out.println("Method 2, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }

    // Efficient for analyzing all sequences
    {
      long highestNumber = 1;
      long highestCount = 1;
      Map<Long, Integer> sequenceMap = new HashMap<Long, Integer>();
      sequenceMap.put(Long.valueOf(1), Integer.valueOf(1));

      List<Long> currentList = new ArrayList<Long>();
      for (long i = 2; i < MAX; i++) {
        currentList.clear();
        Long n = Long.valueOf(i);
        Integer count = null;
        while ((count = sequenceMap.get(n)) == null) {
          currentList.add(n);
          long nValue = n.longValue();
          if ((nValue & 1) == 0)
            n = Long.valueOf(nValue / 2);
          else
            n = Long.valueOf(3 * nValue + 1);
        }
        int curCount = count.intValue();
        for (int j = currentList.size() - 1; j >= 0; j--)
          sequenceMap.put(currentList.get(j), Integer.valueOf(++curCount));
        if (curCount > highestCount) {
          highestCount = curCount;
          highestNumber = i;
        }
      }
      System.out.println("Method 3, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }
    return;
  }
}
