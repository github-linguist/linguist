import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class SymmetricDifference {
    public static void main(String[] args) {
        Set<String> setA = new HashSet<String>(Arrays.asList("John", "Serena", "Bob", "Mary", "Serena"));
        Set<String> setB = new HashSet<String>(Arrays.asList("Jim", "Mary", "John", "Jim", "Bob"));

        // Present our initial data set
        System.out.println("In set A: " + setA);
        System.out.println("In set B: " + setB);

        // Option 1: union of differences
        // Get our individual differences.
        Set<String> notInSetA = new HashSet<String>(setB);
        notInSetA.removeAll(setA);
        Set<String> notInSetB = new HashSet<String>(setA);
        notInSetB.removeAll(setB);

        // The symmetric difference is the concatenation of the two individual differences
        Set<String> symmetricDifference = new HashSet<String>(notInSetA);
        symmetricDifference.addAll(notInSetB);

        // Option 2: union minus intersection
        // Combine both sets
        Set<String> union = new HashSet<String>(setA);
        union.addAll(setB);

        // Get the intersection
        Set<String> intersection = new HashSet<String>(setA);
        intersection.retainAll(setB);

        // The symmetric difference is the union of the 2 sets minus the intersection
        Set<String> symmetricDifference2 = new HashSet<String>(union);
        symmetricDifference2.removeAll(intersection);

        // Present our results
        System.out.println("Not in set A: " + notInSetA);
        System.out.println("Not in set B: " + notInSetB);
        System.out.println("Symmetric Difference: " + symmetricDifference);
        System.out.println("Symmetric Difference 2: " + symmetricDifference2);
    }
}
