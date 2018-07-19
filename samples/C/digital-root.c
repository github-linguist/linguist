#include <stdio.h>

int droot(long long int x, int base, int *pers)
{
	int d = 0;
	if (pers)
		for (*pers = 0; x >= base; x = d, (*pers)++)
			for (d = 0; x; d += x % base, x /= base);
	else if (x && !(d = x % (base - 1)))
			d = base - 1;

	return d;
}

int main(void)
{
	int i, d, pers;
	long long x[] = {627615, 39390, 588225, 393900588225LL};

	for (i = 0; i < 4; i++) {
		d = droot(x[i], 10, &pers);
		printf("%lld: pers %d, root %d\n", x[i], pers, d);
	}

	return 0;
}
