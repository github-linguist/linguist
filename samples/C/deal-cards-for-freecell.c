#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

wchar_t s_suits[] = L"♣♦♥♠", s_nums[] = L"A23456789TJQK";

#define RMAX32 ((1U << 31) - 1)
static int seed = 1;
int  rnd(void) { return (seed = (seed * 214013 + 2531011) & RMAX32) >> 16; }
void srnd(int x) { seed = x; }

void show(const int *c)
{
	int i;
	for (i = 0; i < 52; c++) {
		printf("  \033[%dm%lc\033[m%lc", 32 - (1 + *c) % 4 / 2,
			s_suits[*c % 4], s_nums[*c / 4]);
		if (!(++i % 8) || i == 52) putchar('\n');
	}
}

void deal(int s, int *t)
{
	int i, j;
	srnd(s);

	for (i = 0; i < 52; i++) t[i] = 51 - i;
	for (i = 0; i < 51; i++) {
		j = 51 - rnd() % (52 - i);
		s = t[i], t[i] = t[j], t[j] = s;
	}
}

int main(int c, char **v)
{
	int s, card[52];
	if (c < 2 || (s = atoi(v[1])) <= 0) s = 11982;

	setlocale(LC_ALL, "");

	deal(s, card);
	printf("Hand %d\n", s);
	show(card);

	return 0;
}
