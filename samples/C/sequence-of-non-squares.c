#include <math.h>
#include <stdio.h>
#include <assert.h>

int nonsqr(int n) {
    return n + (int)(0.5 + sqrt(n));
    /* return n + (int)round(sqrt(n)); in C99 */
}

int main() {
    int i;

    /* first 22 values (as a list) has no squares: */
    for (i = 1; i < 23; i++)
        printf("%d ", nonsqr(i));
    printf("\n");

    /* The following check shows no squares up to one million: */
    for (i = 1; i < 1000000; i++) {
        double j = sqrt(nonsqr(i));
        assert(j != floor(j));
    }
    return 0;
}
