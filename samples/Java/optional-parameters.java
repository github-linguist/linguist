import java.util.*;

public class OptionalParams {
    // "natural ordering" comparator
    static <T extends Comparable<? super T>> Comparator<T> naturalOrdering() {
        return Collections.reverseOrder(Collections.<T>reverseOrder());
    }

    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table) {
        sortTable(table, 0);
    }
    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table,
                                       int column) {
        sortTable(table, column, false);
    }
    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table,
                                       int column, boolean reverse) {
        sortTable(table, column, reverse, OptionalParams.<T>naturalOrdering());
    }
    public static <T> void sortTable(T[][] table,
                                     final int column,
                                     final boolean reverse,
                                     final Comparator<T> ordering) {
        Comparator<T[]> myCmp = new Comparator<T[]>() {
            public int compare(T[] x, T[] y) {
                return (reverse ? -1 : 1) *
                       ordering.compare(x[column], y[column]);
            }
        };
        Arrays.sort(table, myCmp);
    }

    public static void main(String[] args) {
        String[][] data0 = {{"a", "b", "c"},
                            {"", "q", "z"},
                            {"zap", "zip", "Zot"}};
        System.out.println(Arrays.deepToString(data0));
        // prints: [[a, b, c], [, q, z], [zap, zip, Zot]]

        // we copy it so that we don't change the original copy
        String[][] data = data0.clone();
        sortTable(data);
        System.out.println(Arrays.deepToString(data));
        // prints: [[, q, z], [a, b, c], [zap, zip, Zot]]

        data = data0.clone();
        sortTable(data, 2);
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [a, b, c], [, q, z]]

        data = data0.clone();
        sortTable(data, 1);
        System.out.println(Arrays.deepToString(data));
        // prints: [[a, b, c], [, q, z], [zap, zip, Zot]]

        data = data0.clone();
        sortTable(data, 1, true);
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [, q, z], [a, b, c]]

        data = data0.clone();
        sortTable(data, 0, false, new Comparator<String>() {
                public int compare(String a, String b) {
                    return b.length() - a.length();
                }
            });
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [a, b, c], [, q, z]]
    }
}
