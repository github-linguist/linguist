#include <stdio.h>
#include <stdlib.h>

int even_sel(int x) { return !(x & 1); }
int tri_sel(int x) { return x % 3; }

/* using a predicate function sel() to select elements */
int* grep(int *in, int len, int *outlen, int (*sel)(int), int inplace)
{
	int i, j, *out;

	if (inplace)	out = in;
	else		out = malloc(sizeof(int) * len);

	for (i = j = 0; i < len; i++)
		if (sel(in[i]))
			out[j++] = in[i];

	if (!inplace && j < len)
		out = realloc(out, sizeof(int) * j);

	*outlen = j;
	return out;
}

int main()
{
	int in[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	int i, len;

	int *even = grep(in, 10, &len, even_sel, 0);
	printf("Filtered even:");
	for (i = 0; i < len; i++) printf(" %d", even[i]);
	printf("\n");

	grep(in, 8, &len, tri_sel, 1);
	printf("In-place filtered not multiple of 3:");
	for (i = 0; i < len; i++) printf(" %d", in[i]);

	printf("\n");

	return 0;
}
