import java.util.*;

class DinesmanMultipleDwelling
{
  private static void generatePermutations(String[] apartmentDwellers, Set<String> set, String curPermutation)
  {
    for (String s : apartmentDwellers)
    {
      if (!curPermutation.contains(s))
      {
        String nextPermutation = curPermutation + s;
        if (nextPermutation.length() == apartmentDwellers.length)
          set.add(nextPermutation);
        else
          generatePermutations(apartmentDwellers, set, nextPermutation);
      }
    }
    return;
  }

  private static boolean topFloor(String permutation, String person)
  {  return permutation.endsWith(person);  }

  private static boolean bottomFloor(String permutation, String person)
  {  return permutation.startsWith(person);  }

  public static boolean livesAbove(String permutation, String upperPerson, String lowerPerson)
  {  return permutation.indexOf(upperPerson) > permutation.indexOf(lowerPerson);  }

  public static boolean adjacent(String permutation, String person1, String person2)
  {  return (Math.abs(permutation.indexOf(person1) - permutation.indexOf(person2)) == 1);  }

  private static boolean isPossible(String s)
  {
    // Conditions here
    if (topFloor(s, "B"))
      return false;
    if (bottomFloor(s, "C"))
      return false;
    if (topFloor(s, "F") || bottomFloor(s, "F"))
      return false;
    if (!livesAbove(s, "M", "C"))
      return false;
    if (adjacent(s, "S", "F"))
      return false;
    if (adjacent(s, "F", "C"))
      return false;
    return true;
  }

  public static void main(String[] args)
  {
    Set<String> set = new HashSet<String>();
    generatePermutations(new String[] { "B", "C", "F", "M", "S" }, set, "");
    for (Iterator<String> iterator = set.iterator(); iterator.hasNext(); )
    {
      String permutation = iterator.next();
      if (!isPossible(permutation))
        iterator.remove();
    }
    for (String s : set)
      System.out.println("Possible arrangement: " + s);
  }
}
