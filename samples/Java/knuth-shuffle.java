import java.util.Random;

public static final Random gen = new Random();

// version for array of ints
public static void shuffle (int[] array) {
    int n = array.length;
    while (n > 1) {
        int k = gen.nextInt(n--); //decrements after using the value
        int temp = array[n];
        array[n] = array[k];
        array[k] = temp;
    }
}
// version for array of references
public static void shuffle (Object[] array) {
    int n = array.length;
    while (n > 1) {
        int k = gen.nextInt(n--); //decrements after using the value
        Object temp = array[n];
        array[n] = array[k];
        array[k] = temp;
    }
}
