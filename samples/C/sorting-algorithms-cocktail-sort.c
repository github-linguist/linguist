#include <stdio.h>

#define try_swap { if (a[i] < a[i - 1])\
	{ t = a[i]; a[i] = a[i - 1]; a[i - 1] = t; t = 0;} }

void cocktailsort(int *a, size_t len)
{
	size_t i;
	int t = 0;
	while (!t) {
		for (i = 1, t = 1; i < len; i++) try_swap;
		if (t) break;
		for (i = len - 1, t = 1; i; i--) try_swap;
	}
}

int main()
{
	int x[] = { 5, -1, 101, -4, 0, 1, 8, 6, 2, 3 };
	size_t i, len = sizeof(x)/sizeof(x[0]);

	cocktailsort(x, len);
	for (i = 0; i < len; i++)
		printf("%d\n", x[i]);
	return 0;
}
