public class SeqNonSquares {
    public static int nonsqr(int n) {
        return n + (int)Math.round(Math.sqrt(n));
    }

    public static void main(String[] args) {
        // first 22 values (as a list) has no squares:
        for (int i = 1; i < 23; i++)
            System.out.print(nonsqr(i) + " ");
        System.out.println();

        // The following check shows no squares up to one million:
        for (int i = 1; i < 1000000; i++) {
            double j = Math.sqrt(nonsqr(i));
            assert j != Math.floor(j);
        }
    }
}
