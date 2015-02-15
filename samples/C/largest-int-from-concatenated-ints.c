#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int catcmp(const void *a, const void *b)
{
	char ab[32], ba[32];
	sprintf(ab, "%d%d", *(int*)a, *(int*)b);
	sprintf(ba, "%d%d", *(int*)b, *(int*)a);
	return strcmp(ba, ab);
}

void maxcat(int *a, int len)
{
	int i;
	qsort(a, len, sizeof(int), catcmp);
	for (i = 0; i < len; i++)
		printf("%d", a[i]);
	putchar('\n');
}

int main(void)
{
	int x[] = {1, 34, 3, 98, 9, 76, 45, 4};
	int y[] = {54, 546, 548, 60};

	maxcat(x, sizeof(x)/sizeof(x[0]));
	maxcat(y, sizeof(y)/sizeof(y[0]));

	return 0;
}
