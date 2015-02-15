#include <stdio.h>
typedef unsigned long long LONG;

LONG deranged(int depth, int len, int *d, int show)
{
        int i;
        char tmp;
        LONG count = 0;

        if (depth == len) {
                if (show) {
                        for (i = 0; i < len; i++) putchar(d[i] + 'a');
                        putchar('\n');
                }
                return 1;
        }
        for (i = len - 1; i >= depth; i--) {
                if (i == d[depth]) continue;

                tmp = d[i]; d[i] = d[depth]; d[depth] = tmp;
                count += deranged(depth + 1, len, d, show);
                tmp = d[i]; d[i] = d[depth]; d[depth] = tmp;
        }
        return count;
}

LONG gen_n(int n, int show)
{
        LONG i;
        int a[1024]; /* 1024 ought to be big enough for anybody */

        for (i = 0; i < n; i++) a[i] = i;
        return deranged(0, n, a, show);
}

LONG sub_fact(int n)
{
        return n < 2 ? 1 - n : (sub_fact(n - 1) + sub_fact(n - 2)) * (n - 1);
}

int main()
{
        int i;

        printf("Deranged Four:\n");
        gen_n(4, 1);

        printf("\nCompare list vs calc:\n");
        for (i = 0; i < 10; i++)
                printf("%d:\t%llu\t%llu\n", i, gen_n(i, 0), sub_fact(i));

        printf("\nfurther calc:\n");
        for (i = 10; i <= 20; i++)
                printf("%d: %llu\n", i, sub_fact(i));

        return 0;
}
