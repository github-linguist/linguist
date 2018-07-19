#include <stdio.h>
#include <stdlib.h>

#define N 100000
int main()
{
	int i, flip, *q = (int*)malloc(sizeof(int) * N) - 1;

	q[1] = q[2] = 1;

	for (i = 3; i <= N; i++)
		q[i] = q[i - q[i - 1]] + q[i - q[i - 2]];
		
	for (i = 1; i <= 10; i++)
		printf("%d%c", q[i], i == 10 ? '\n' : ' ');

	printf("%d\n", q[1000]);

	for (flip = 0, i = 1; i < N; i++)
		flip += q[i] > q[i + 1];

	printf("flips: %d\n", flip);
	return 0;
}
