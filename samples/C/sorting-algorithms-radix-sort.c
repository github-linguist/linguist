#include <stdio.h>
#include <limits.h>
#include <stdlib.h>

typedef unsigned uint;
#define swap(a, b) { tmp = a; a = b; b = tmp; }
#define each(i, x) for (i = 0; i < x; i++)

/* sort unsigned ints */
static void rad_sort_u(uint *from, uint *to, uint bit)
{
	if (!bit || to < from + 1) return;

	uint *ll = from, *rr = to - 1, tmp;
	while (1) {
		/* find left most with bit, and right most without bit, swap */
		while (ll < rr && !(*ll & bit)) ll++;
		while (ll < rr &&  (*rr & bit)) rr--;
		if (ll >= rr) break;
		swap(*ll, *rr);
	}

	if (!(bit & *ll) && ll < to) ll++;
	bit >>= 1;

	rad_sort_u(from, ll, bit);
	rad_sort_u(ll, to, bit);
}

/* sort signed ints: flip highest bit, sort as unsigned, flip back */
static void radix_sort(int *a, const size_t len)
{
	size_t i;
	uint *x = (uint*) a;

	each(i, len) x[i] ^= INT_MIN;
	rad_sort_u(x, x + len, INT_MIN);
	each(i, len) x[i] ^= INT_MIN;
}

static inline void radix_sort_unsigned(uint *a, const size_t len)
{
	rad_sort_u(a, a + len, (uint)INT_MIN);
}

int main(void)
{
	int len = 16, x[16], i;
	size_t len = 16, i;
	each(i, len) x[i] = rand() % 512 - 256;

	radix_sort(x, len);

	each(i, len) printf("%d%c", x[i], i + 1 < len ? ' ' : '\n');

	return 0;
}
