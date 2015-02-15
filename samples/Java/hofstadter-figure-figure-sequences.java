import java.util.*;

class Hofstadter
{
  private static List<Integer> getSequence(int rlistSize, int slistSize)
  {
    List<Integer> rlist = new ArrayList<Integer>();
    List<Integer> slist = new ArrayList<Integer>();
    Collections.addAll(rlist, 1, 3, 7);
    Collections.addAll(slist, 2, 4, 5, 6);
    List<Integer> list = (rlistSize > 0) ? rlist : slist;
    int targetSize = (rlistSize > 0) ? rlistSize : slistSize;
    while (list.size() > targetSize)
      list.remove(list.size() - 1);
    while (list.size() < targetSize)
    {
      int lastIndex = rlist.size() - 1;
      int lastr = rlist.get(lastIndex).intValue();
      int r = lastr + slist.get(lastIndex).intValue();
      rlist.add(Integer.valueOf(r));
      for (int s = lastr + 1; (s < r) && (list.size() < targetSize); s++)
        slist.add(Integer.valueOf(s));
    }
    return list;
  }

  public static int ffr(int n)
  {  return getSequence(n, 0).get(n - 1).intValue();  }

  public static int ffs(int n)
  {  return getSequence(0, n).get(n - 1).intValue();  }

  public static void main(String[] args)
  {
    System.out.print("R():");
    for (int n = 1; n <= 10; n++)
      System.out.print(" " + ffr(n));
    System.out.println();

    Set<Integer> first40R = new HashSet<Integer>();
    for (int n = 1; n <= 40; n++)
      first40R.add(Integer.valueOf(ffr(n)));

    Set<Integer> first960S = new HashSet<Integer>();
    for (int n = 1; n <= 960; n++)
      first960S.add(Integer.valueOf(ffs(n)));

    for (int i = 1; i <= 1000; i++)
    {
      Integer n = Integer.valueOf(i);
      if (first40R.contains(n) == first960S.contains(n))
        System.out.println("Integer " + i + " either in both or neither set");
    }
    System.out.println("Done");
  }
}
