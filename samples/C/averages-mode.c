#include <stdio.h>
#include <stdlib.h>

typedef struct { double v; int c; } vcount;

int cmp_dbl(const void *a, const void *b)
{
	double x = *(const double*)a - *(const double*)b;
	return x < 0 ? -1 : x > 0;
}

int vc_cmp(const void *a, const void *b)
{
	return ((const vcount*)b)->c - ((const vcount*)a)->c;
}

int get_mode(double* x, int len, vcount **list)
{
	int i, j;
	vcount *vc;

	/* sort values */
	qsort(x, len, sizeof(double), cmp_dbl);

	/* count occurence of each value */
	for (i = 0, j = 1; i < len - 1; i++, j += (x[i] != x[i + 1]));

	*list = vc = malloc(sizeof(vcount) * j);
	vc[0].v = x[0];
	vc[0].c = 1;

	/* generate list value-count pairs */
	for (i = j = 0; i < len - 1; i++, vc[j].c++)
		if (x[i] != x[i + 1]) vc[++j].v = x[i + 1];

	/* sort that by count in descending order */
	qsort(vc, j + 1, sizeof(vcount), vc_cmp);

	/* the number of entries with same count as the highest */
	for (i = 0; i <= j && vc[i].c == vc[0].c; i++);

	return i;
}

int main()
{
	double values[] = { 1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 12, 12, 17 };
#	define len sizeof(values)/sizeof(double)
	vcount *vc;

	int i, n_modes = get_mode(values, len, &vc);

	printf("got %d modes:\n", n_modes);
	for (i = 0; i < n_modes; i++)
		printf("\tvalue = %g, count = %d\n", vc[i].v, vc[i].c);

	free(vc);
	return 0;
}
