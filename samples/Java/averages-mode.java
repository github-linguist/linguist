import java.util.*;

public class Mode {
    public static <T> List<T> mode(List<? extends T> coll) {
        Map<T, Integer> seen = new HashMap<T, Integer>();
        int max = 0;
        List<T> maxElems = new ArrayList<T>();
        for (T value : coll) {
            if (seen.containsKey(value))
                seen.put(value, seen.get(value) + 1);
            else
                seen.put(value, 1);
            if (seen.get(value) > max) {
                max = seen.get(value);
                maxElems.clear();
                maxElems.add(value);
            } else if (seen.get(value) == max) {
                maxElems.add(value);
            }
        }
        return maxElems;
    }

    public static void main(String[] args) {
        System.out.println(mode(Arrays.asList(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17))); // prints [6]
        System.out.println(mode(Arrays.asList(1, 1, 2, 4, 4))); // prints [1, 4]
    }
}
