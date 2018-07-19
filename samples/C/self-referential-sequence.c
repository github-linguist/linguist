#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct rec_t rec_t;
struct rec_t {
	int depth;
	rec_t * p[10];
};

rec_t root = {0, {0}};

#define USE_POOL_ALLOC
#ifdef USE_POOL_ALLOC /* not all that big a deal */
rec_t *tail = 0, *head = 0;
#define POOL_SIZE (1 << 20)
inline rec_t *new_rec()
{
	if (head == tail) {
		head = calloc(sizeof(rec_t), POOL_SIZE);
		tail = head + POOL_SIZE;
	}
	return head++;
}
#else
#define new_rec() calloc(sizeof(rec_t), 1)
#endif

rec_t *find_rec(char *s)
{
	int i;
	rec_t *r = &root;
	while (*s) {
		i = *s++ - '0';
		if (!r->p[i]) r->p[i] = new_rec();
		r = r->p[i];
	}
	return r;
}

/* speed up number to string conversion */
char number[100][4];
void init()
{
	int i;
	for (i = 0; i < 100; i++)
		sprintf(number[i], "%d", i);
}

void count(char *buf)
{
	int i, c[10] = {0};
	char *s;

	for (s = buf; *s; c[*s++ - '0']++);

	for (i = 9; i >= 0; i--) {
		if (!c[i]) continue;
		s = number[c[i]];

		*buf++ = s[0];
		if ((*buf = s[1])) buf++;

		*buf++ = i + '0';
	}

	*buf = '\0';
}

int depth(char *in, int d)
{
	rec_t *r = find_rec(in);

	if (r->depth > 0)
		return r->depth;

	d++;
	if (!r->depth)	r->depth = -d;
	else		r->depth += d;

	count(in);
	d = depth(in, d);

	if (r->depth <= 0) r->depth = d + 1;
	return r->depth;
}

int main(void)
{
	char a[100];
	int i, d, best_len = 0, n_best = 0;
	int best_ints[32];
	rec_t *r;

	init();

	for (i = 0; i < 1000000; i++) {
		sprintf(a, "%d", i);
		d = depth(a, 0);

		if (d < best_len) continue;
		if (d > best_len) {
			n_best = 0;
			best_len = d;
		}
		if (d == best_len)
			best_ints[n_best++] = i;
	}

	printf("longest length: %d\n", best_len);
	for (i = 0; i < n_best; i++) {
		printf("%d\n", best_ints[i]);
		sprintf(a, "%d", best_ints[i]);
		for (d = 0; d <= best_len; d++) {
			r = find_rec(a);
			printf("%3d: %s\n", r->depth, a);
			count(a);
		}
		putchar('\n');
	}

	return 0;
}
