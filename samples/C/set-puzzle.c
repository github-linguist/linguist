#include <stdio.h>
#include <stdlib.h>

char *names[4][3] = {
	{ "red", "green", "purple" },
	{ "oval", "squiggle", "diamond" },
	{ "one", "two", "three" },
	{ "solid", "open", "striped" }
};

int set[81][81];

void init_sets(void)
{
	int i, j, t, a, b;
	for (i = 0; i < 81; i++) {
		for (j = 0; j < 81; j++) {
			for (t = 27; t; t /= 3) {
				a = (i / t) % 3;
				b = (j / t) % 3;
				set[i][j] += t * (a == b ? a : 3 - a - b);
			}
		}
	}
}

void deal(int *out, int n)
{
	int i, j, t, c[81];
	for (i = 0; i < 81; i++) c[i] = i;
	for (i = 0; i < n; i++) {
		j = i + (rand() % (81 - i));
		t = c[i], c[i] = out[i] = c[j], c[j] = t;
	}
}

int get_sets(int *cards, int n, int sets[][3])
{
	int i, j, k, s = 0;
	for (i = 0; i < n; i++) {
		for (j = i + 1; j < n; j++) {
			for (k = j + 1; k < n; k++) {
				if (set[cards[i]][cards[j]] == cards[k])
					sets[s][0] = i,
					sets[s][1] = j,
					sets[s][2] = k,
					s++;
			}
		}
	}

	return s;
}

void show_card(int c)
{
	int i, t;
	for (i = 0, t = 27; t; i++, t /= 3)
		printf("%9s", names[i][(c/t)%3]);
	putchar('\n');
}

void deal_sets(int ncard, int nset)
{
	int c[81];
	int csets[81][3]; // might not be enough for large ncard
	int i, j, s;

	do deal(c, ncard); while ((s = get_sets(c, ncard, csets)) != nset);

	printf("dealt %d cards\n", ncard);
	for (i = 0; i < ncard; i++) {
		printf("%2d:", i);
		show_card(c[i]);
	}
	printf("\nsets:\n");

	for (i = 0; i < s; i++) {
		for (j = 0; j < 3; j++) {
			printf("%2d:", csets[i][j]);
			show_card(c[csets[i][j]]);
		}
		putchar('\n');
	}
}

int main(void)
{
	init_sets();
	deal_sets(9, 4);

	while (1) deal_sets(12, 6);

	return 0;
}
