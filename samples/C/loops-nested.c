#include <stdlib.h>
#include <time.h>
#include <stdio.h>

int main() {
    int a[10][10], i, j;

    srand(time(NULL));
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            a[i][j] = rand() % 20 + 1;

    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {
            printf(" %d", a[i][j]);
            if (a[i][j] == 20)
                goto Done;
        }
        printf("\n");
    }
Done:
    printf("\n");
    return 0;
}
