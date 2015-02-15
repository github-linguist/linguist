#include <stdlib.h>
#include <stdio.h>
#include <math.h>

inline int rand5()
{
	int r, rand_max = RAND_MAX - (RAND_MAX % 5);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / 5) + 1;
}

inline int rand5_7()
{
	int r;
	while ((r = rand5() * 5 + rand5()) >= 27);
	return r / 3 - 1;
}

/* assumes gen() returns a value from 1 to n */
int check(int (*gen)(), int n, int cnt, double delta) /* delta is relative */
{
	int i = cnt, *bins = calloc(sizeof(int), n);
	double ratio;
	while (i--) bins[gen() - 1]++;
	for (i = 0; i < n; i++) {
		ratio = bins[i] * n / (double)cnt - 1;
		if (ratio > -delta && ratio < delta) continue;

		printf("bin %d out of range: %d (%g%% vs %g%%), ",
			i + 1, bins[i], ratio * 100, delta * 100);
		break;
	}
	free(bins);
	return i == n;
}

int main()
{
	int cnt = 1;
	while ((cnt *= 10) <= 1000000) {
		printf("Count = %d: ", cnt);
		printf(check(rand5_7, 7, cnt, 0.03) ? "flat\n" : "NOT flat\n");
	}

	return 0;
}
