#include <stdlib.h>
#include <time.h>
#include <stdio.h>

int main() {
    int a, b;

    srand(time(NULL));
    while (1) {
        a = rand() % 20; /* not exactly uniformly distributed, but doesn't matter */
        printf("%d\n", a);
        if (a == 10) break;
        b = rand() % 20; /* not exactly uniformly distributed, but doesn't matter */
        printf("%d\n", b);
    }
    return 0;
}
