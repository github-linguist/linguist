public static <E extends Comparable<? super E>> void sort(E[] input) {
    int gap = input.length;
    boolean swapped = true;
    while (gap > 1 || swapped) {
        if (gap > 1) {
            gap = (int) (gap / 1.3);
        }
        swapped = false;
        for (int i = 0; i + gap < input.length; i++) {
            if (input[i].compareTo(input[i + gap]) > 0) {
                E t = input[i];
                input[i] = input[i + gap];
                input[i + gap] = t;
                swapped = true;
            }
        }
    }
}
