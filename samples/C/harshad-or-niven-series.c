#include <stdio.h>

static int digsum(int n)
{
    int sum = 0;
    do { sum += n % 10; } while (n /= 10);
    return sum;
}

int main(void)
{
    int n, done, found;

    for (n = 1, done = found = 0; !done; ++n) {
        if (n % digsum(n) == 0) {
            if (found++ < 20) printf("%d ", n);
            if (n > 1000) done = printf("\n%d\n", n);
        }
    }

    return 0;
}
