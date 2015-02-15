#include <stdio.h>
#include <stdlib.h>

int a_list[1<<20 + 1];

int doSqnc( int m)
{
    int max_df = 0;
    int p2_max = 2;
    int v, n;
    int k1 = 2;
    int lg2 = 1;
    double amax = 0;
    a_list[0] = -50000;
    a_list[1] = a_list[2] = 1;
    v = a_list[2];

    for (n=3; n <= m;  n++) {
        v = a_list[n] = a_list[v] + a_list[n-v];
        if ( amax < v*1.0/n) amax = v*1.0/n;
        if ( 0 == (k1&n)) {
            printf("Maximum between 2^%d and 2^%d was %f\n", lg2,lg2+1, amax);
            amax = 0;
            lg2++;
        }
        k1 = n;
    }
    return 1;
}
