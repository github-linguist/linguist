#include <stdio.h>
//~ Take a number n and return a function that takes a number i
#define ACCUMULATOR(name,n) __typeof__(n) name (__typeof__(n) i) { \
    static __typeof__(n) _n=n; LOGIC; }
//~ have it return n incremented by the accumulation of i
#define LOGIC return _n+=i
ACCUMULATOR(x,1.0)
ACCUMULATOR(y,3)
ACCUMULATOR(z,'a')
#undef LOGIC
int main (void) {
    printf ("%f\n", x(5));   /* 6.000000 */
    printf ("%f\n", x(2.3)); /* 8.300000 */
    printf ("%i\n", y(5.0)); /* 8 */
    printf ("%i\n", y(3.3)); /* 11 */
    printf ("%c\n", z(5));   /* f */
    return 0;
}
