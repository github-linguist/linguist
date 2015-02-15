import std.random;
import verify_distribution_uniformity_naive: distCheck;

/// Generates a random number in [1, 5].
int dice5() /*pure nothrow*/ {
    return uniform(1, 6);
}

/// Naive, generates a random number in [1, 7] using dice5.
int fiveToSevenNaive() /*pure nothrow*/ {
    immutable int r = dice5() + dice5() * 5 - 6;
    return (r < 21) ? (r % 7) + 1 : fiveToSevenNaive();
}

/**
Generates a random number in [1, 7] using dice5,
minimizing calls to dice5.
*/
int fiveToSevenSmart() {
    static int rem = 0, max = 1;

    while (rem / 7 == max / 7) {
        while (max < 7) {
            immutable int rand5 = dice5() - 1;
            max *= 5;
            rem = 5 * rem + rand5;
        }

        immutable int groups = max / 7;
        if (rem >= 7 * groups) {
            rem -= 7 * groups;
            max -= 7 * groups;
        }
    }

    immutable int result = rem % 7;
    rem /= 7;
    max /= 7;
    return result + 1;
}

void main() {
    enum int N = 400_000;
    distCheck(&dice5, N, 1);
    distCheck(&fiveToSevenNaive, N, 1);
    distCheck(&fiveToSevenSmart, N, 1);
}
