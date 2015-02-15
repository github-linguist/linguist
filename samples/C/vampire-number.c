#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

typedef uint64_t xint;
typedef unsigned long long ull;

xint tens[20];

inline xint max(xint a, xint b) { return a > b ? a : b; }
inline xint min(xint a, xint b) { return a < b ? a : b; }
inline int ndigits(xint x)
{
	int n = 0;
	while (x) n++, x /= 10;
	return n;
}

inline xint dtally(xint x)
{
	xint t = 0;
	while (x) t += 1<<((x%10) * 6), x /= 10;

	return t;
}

int fangs(xint x, xint *f)
{
	int n = 0;
	int nd = ndigits(x);
	if (nd & 1) return 0;
	nd /= 2;

	xint lo, hi;
	lo = max(tens[nd-1], (x + tens[nd] - 2)/ (tens[nd] - 1));
	hi = min(x / lo, sqrt(x));

	xint a, b, t = dtally(x);
	for (a = lo; a <= hi; a++) {
		b = x / a;
		if (a * b == x && ((a%10) || (b%10)) && t == dtally(a) + dtally(b))
			f[n++] = a;
	}

	return n;
}

void show_fangs(xint x, xint *f, xint cnt)
{
	printf("%llu", (ull)x);
	int i;
	for (i = 0; i < cnt; i++)
		printf(" = %llu x %llu", (ull)f[i], (ull)(x / f[i]));
	putchar('\n');
}

int main(void)
{
	int i, j, n;
	xint x, f[16], bigs[] = {16758243290880ULL, 24959017348650ULL, 14593825548650ULL, 0};

	tens[0] = 1;
	for (i = 1; i < 20; i++)
		tens[i] = tens[i-1] * 10;

	for (x = 1, n = 0; n < 25; x++) {
		if (!(j = fangs(x, f))) continue;
		printf("%2d: ", ++n);
		show_fangs(x, f, j);
	}

	putchar('\n');
	for (i = 0; bigs[i]; i++) {
		if ((j = fangs(bigs[i], f)))
			show_fangs(bigs[i], f, j);
		else
			printf("%llu is not vampiric\n", (ull)bigs[i]);
	}

	return 0;
}
