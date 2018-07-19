#include <stdio.h>

#define JOBS 12
#define jobs(a) for (switch_to(a = 0); a < JOBS || !printf("\n"); switch_to(++a))
typedef struct { int seq, cnt; } env_t;

env_t env[JOBS] = {{0, 0}};
int *seq, *cnt;

void hail()
{
	printf("% 4d", *seq);
	if (*seq == 1) return;
	++*cnt;
	*seq = (*seq & 1) ? 3 * *seq + 1 : *seq / 2;
}

void switch_to(int id)
{
	seq = &env[id].seq;
	cnt = &env[id].cnt;
}

int main()
{
	int i;
	jobs(i) { env[i].seq = i + 1; }

again:	jobs(i) { hail(); }
	jobs(i) { if (1 != *seq) goto again; }

	printf("COUNTS:\n");
	jobs(i) { printf("% 4d", *cnt); }

	return 0;
}
