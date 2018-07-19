#include <stdio.h>
#include <stdlib.h>

// simple extensible array stuff
typedef unsigned long long xint;

typedef struct {
	size_t len, alloc;
	xint *buf;
} xarray;

xarray rs, ss;

void setsize(xarray *a, size_t size)
{
	size_t n = a->alloc;
	if (!n) n = 1;

	while (n < size) n <<= 1;
	if (a->alloc < n) {
		a->buf = realloc(a->buf, sizeof(xint) * n);
		if (!a->buf) abort();
		a->alloc = n;
	}
}

void push(xarray *a, xint v)
{
	while (a->alloc <= a->len)
		setsize(a, a->alloc * 2);

	a->buf[a->len++] = v;
}


// sequence stuff
void RS_append(void);

xint R(int n)
{
	while (n > rs.len) RS_append();
	return rs.buf[n - 1];
}

xint S(int n)
{
	while (n > ss.len) RS_append();
	return ss.buf[n - 1];
}

void RS_append()
{
	int n = rs.len;
	xint r = R(n) + S(n);
	xint s = S(ss.len);

	push(&rs, r);
	while (++s < r) push(&ss, s);
	push(&ss, r + 1); // pesky 3
}

int main(void)
{
	push(&rs, 1);
	push(&ss, 2);

	int i;
	printf("R(1 .. 10):");
	for (i = 1; i <= 10; i++)
		printf(" %llu", R(i));

	char seen[1001] = { 0 };
	for (i = 1; i <=  40; i++) seen[ R(i) ] = 1;
	for (i = 1; i <= 960; i++) seen[ S(i) ] = 1;
	for (i = 1; i <= 1000 && seen[i]; i++);

	if (i <= 1000) {
		fprintf(stderr, "%d not seen\n", i);
		abort();
	}

	puts("\nfirst 1000 ok");
	return 0;
}
