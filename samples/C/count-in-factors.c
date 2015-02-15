#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long ULONG;

ULONG get_prime(int idx)
{
        static long n_primes = 0, alloc = 0;
        static ULONG *primes = 0;
        ULONG last, p;
        int i;

        if (idx >= n_primes) {
                if (n_primes >= alloc) {
                        alloc += 16; /* be conservative */
                        primes = realloc(primes, sizeof(ULONG) * alloc);
                }
                if (!n_primes) {
                        primes[0] = 2;
                        primes[1] = 3;
                        n_primes = 2;
                }

                last = primes[n_primes-1];
                while (idx >= n_primes) {
                        last += 2;
                        for (i = 0; i < n_primes; i++) {
                                p = primes[i];
                                if (p * p > last) {
                                        primes[n_primes++] = last;
                                        break;
                                }
                                if (last % p == 0) break;
                        }
                }
        }
        return primes[idx];
}

int main()
{
        ULONG n, x, p;
        int i, first;

        for (x = 1; ; x++) {
                printf("%lld = ", n = x);

                for (i = 0, first = 1; ; i++) {
                        p = get_prime(i);
                        while (n % p == 0) {
                                n /= p;
                                if (!first) printf(" x ");
                                first = 0;
                                printf("%lld", p);
                        }
                        if (n <= p * p) break;
                }

                if (first)      printf("%lld\n", n);
                else if (n > 1) printf(" x %lld\n", n);
                else            printf("\n");
        }
        return 0;
}
