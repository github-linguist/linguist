#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef unsigned char cell;
int dx[] = { -2, -2, -1, 1, 2,  2,  1, -1 };
int dy[] = { -1,  1,  2, 2, 1, -1, -2, -2 };

void init_board(int w, int h, cell **a, cell **b)
{
	int i, j, k, x, y, p = w + 4, q = h + 4;
	/* b is board; a is board with 2 rows padded at each side */
	a[0] = (cell*)(a + q);
	b[0] = a[0] + 2;

	for (i = 1; i < q; i++) {
		a[i] = a[i-1] + p;
		b[i] = a[i] + 2;
	}

	memset(a[0], 255, p * q);
	for (i = 0; i < h; i++) {
		for (j = 0; j < w; j++) {
			for (k = 0; k < 8; k++) {
				x = j + dx[k], y = i + dy[k];
				if (b[i+2][j] == 255) b[i+2][j] = 0;
				b[i+2][j] += x >= 0 && x < w && y >= 0 && y < h;
			}
		}
	}
}

#define E "\033["
int walk_board(int w, int h, int x, int y, cell **b)
{
	int i, nx, ny, least;
	int steps = 0;
	printf(E"H"E"J"E"%d;%dH"E"32m[]"E"m", y + 1, 1 + 2 * x);

	while (1) {
		/* occupy cell */
		b[y][x] = 255;

		/* reduce all neighbors' neighbor count */
		for (i = 0; i < 8; i++)
			b[ y + dy[i] ][ x + dx[i] ]--;

		/* find neighbor with lowest neighbor count */
		least = 255;
		for (i = 0; i < 8; i++) {
			if (b[ y + dy[i] ][ x + dx[i] ] < least) {
				nx = x + dx[i];
				ny = y + dy[i];
				least = b[ny][nx];
			}
		}

		if (least > 7) {
			printf(E"%dH", h + 2);
			return steps == w * h - 1;
		}

		if (steps++) printf(E"%d;%dH[]", y + 1, 1 + 2 * x);
		x = nx, y = ny;
		printf(E"%d;%dH"E"31m[]"E"m", y + 1, 1 + 2 * x);
		fflush(stdout);
		usleep(120000);
	}
}

int solve(int w, int h)
{
	int x = 0, y = 0;
	cell **a, **b;
	a = malloc((w + 4) * (h + 4) + sizeof(cell*) * (h + 4));
	b = malloc((h + 4) * sizeof(cell*));

	while (1) {
		init_board(w, h, a, b);
		if (walk_board(w, h, x, y, b + 2)) {
			printf("Success!\n");
			return 1;
		}
		if (++x >= w) x = 0, y++;
		if (y >= h) {
			printf("Failed to find a solution\n");
			return 0;
		}
		printf("Any key to try next start position");
		getchar();
	}
}

int main(int c, char **v)
{
	int w, h;
	if (c < 2 || (w = atoi(v[1])) <= 0) w = 8;
	if (c < 3 || (h = atoi(v[2])) <= 0) h = w;
	solve(w, h);

	return 0;
}
