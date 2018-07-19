#include <stdlib.h>
#include <math.h>

char* eratosthenes (int n, int *c)
{
  char* sieve;
  int i, j, m;

  if (n < 2) { *c = 0; return NULL; }

  *c = n-1;     /* primes count */
  m = (int) sqrt ((double) n);

  /* calloc initializes to zero */
  sieve = calloc (n+1, sizeof(char));
  sieve[0] = 1;
  sieve[1] = 1;
  for (i = 2; i <= m; i++) {
    if (sieve[i] == 0) {
      for (j = i*i; j <= n; j += i) {
        if (sieve[j] == 0) {
          sieve[j] = 1;
          --(*c);
        }
      }
    }
  }
  return sieve;
}
