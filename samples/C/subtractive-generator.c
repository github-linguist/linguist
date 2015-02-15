#include<stdio.h>

#define MOD 1000000000
int state[55], si = 0, sj = 0;

int subrand();

void subrand_seed(int p1)
{
	int i, j, p2 = 1;

	state[0] = p1 % MOD;
	for (i = 1, j = 21; i < 55; i++, j += 21) {
		if (j >= 55) j -= 55;
		state[j] = p2;
		if ((p2 = p1 - p2) < 0) p2 += MOD;
		p1 = state[j];
	}
	si = 0;
	sj = 24;
	for (i = 0; i < 165; i++) subrand();
}

int subrand()
{
	int x;
	if (si == sj) subrand_seed(0);

	if (!si--) si = 54;
	if (!sj--) sj = 54;
	if ((x = state[si] - state[sj]) < 0) x += MOD;

	return state[si] = x;
}

int main()
{
	subrand_seed(292929);
	int i;
	for (i = 0; i < 10; i++) printf("%d\n", subrand());

	return 0;
}
