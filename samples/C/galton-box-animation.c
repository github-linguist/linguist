#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BALLS 1024
int n, w, h = 45, *x, *y, cnt = 0;
char *b;

#define B(y, x) b[(y)*w + x]
#define C(y, x) ' ' == b[(y)*w + x]
#define V(i) B(y[i], x[i])
inline int rnd(int a) { return (rand()/(RAND_MAX/a))%a; }

void show_board()
{
	int i, j;
	for (puts("\033[H"), i = 0; i < h; i++, putchar('\n'))
		for (j = 0; j < w; j++, putchar(' '))
			printf(B(i, j) == '*' ?
				C(i - 1, j) ? "\033[32m%c\033[m" :
				"\033[31m%c\033[m" : "%c", B(i, j));
}

void init()
{
	int i, j;
	puts("\033[H\033[J");
	b = malloc(w * h);
	memset(b, ' ', w * h);

	x = malloc(sizeof(int) * BALLS * 2);
	y = x + BALLS;

	for (i = 0; i < n; i++)
		for (j = -i; j <= i; j += 2)
			B(2 * i+2, j + w/2) = '*';
	srand(time(0));
}

void move(int idx)
{
	int xx = x[idx], yy = y[idx], c, kill = 0, sl = 3, o = 0;

	if (yy < 0) return;
	if (yy == h - 1) { y[idx] = -1; return; }

	switch(c = B(yy + 1, xx)) {
	case ' ':	yy++; break;
	case '*':	sl = 1;
	default:	if (xx < w - 1 && C(yy, xx + 1) && C(yy + 1, xx + 1))
				if (!rnd(sl++)) o = 1;
			if (xx && C(yy, xx - 1) && C(yy + 1, xx - 1))
				if (!rnd(sl++)) o = -1;
			if (!o) kill = 1;
			xx += o;
	}

	c = V(idx); V(idx) = ' ';
	idx[y] = yy, idx[x] = xx;
	B(yy, xx) = c;
	if (kill) idx[y] = -1;
}

int run(void)
{
	static int step = 0;
	int i;
	for (i = 0; i < cnt; i++) move(i);
	if (2 == ++step && cnt < BALLS) {
		step = 0;
		x[cnt] = w/2;
		y[cnt] = 0;
		if (V(cnt) != ' ') return 0;
		V(cnt) = rnd(80) + 43;
		cnt++;
	}
	return 1;
}

int main(int c, char **v)
{
	if (c < 2 || (n = atoi(v[1])) <= 3) n = 5;
	if (n >= 20) n = 20;
	w = n * 2 + 1;
	init();

	do { show_board(), usleep(60000); } while (run());

	return 0;
}
