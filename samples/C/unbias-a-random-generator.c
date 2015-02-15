#include <stdio.h>
#include <stdlib.h>

int biased(int bias)
{
	/* balance out the bins, being pedantic */
	int r, rand_max = RAND_MAX - (RAND_MAX % bias);
	while ((r = rand()) > rand_max);
	return r < rand_max / bias;
}

int unbiased(int bias)
{
	int a;
	while ((a = biased(bias)) == biased(bias));
	return a;
}

int main()
{
	int b, n = 10000, cb, cu, i;
	for (b = 3; b <= 6; b++) {
		for (i = cb = cu = 0; i < n; i++) {
			cb += biased(b);
			cu += unbiased(b);
		}
		printf("bias %d: %5.3f%% vs %5.3f%%\n", b,
			100. * cb / n, 100. * cu / n);
	}

	return 0;
}
