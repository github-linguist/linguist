public class Topswops {
    static final int maxBest = 32;
    static int[] best;

    static private void trySwaps(int[] deck, int f, int d, int n) {
        if (d > best[n])
            best[n] = d;

        for (int i = n - 1; i >= 0; i--) {
            if (deck[i] == -1 || deck[i] == i)
                break;
            if (d + best[i] <= best[n])
                return;
        }

        int[] deck2 = deck.clone();
        for (int i = 1; i < n; i++) {
            final int k = 1 << i;
            if (deck2[i] == -1) {
                if ((f & k) != 0)
                    continue;
            } else if (deck2[i] != i)
                continue;

            deck2[0] = i;
            for (int j = i - 1; j >= 0; j--)
                deck2[i - j] = deck[j]; // Reverse copy.
            trySwaps(deck2, f | k, d + 1, n);
        }
    }

    static int topswops(int n) {
        assert(n > 0 && n < maxBest);
        best[n] = 0;
        int[] deck0 = new int[n + 1];
        for (int i = 1; i < n; i++)
            deck0[i] = -1;
        trySwaps(deck0, 1, 0, n);
        return best[n];
    }

    public static void main(String[] args) {
        best = new int[maxBest];
        for (int i = 1; i < 11; i++)
            System.out.println(i + ": " + topswops(i));
    }
}
