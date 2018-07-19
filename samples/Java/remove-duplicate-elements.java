import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;

Object[] data = {1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"};
Set<Object> uniqueSet = new HashSet<Object>(Arrays.asList(data));
Object[] unique = uniqueSet.toArray();
