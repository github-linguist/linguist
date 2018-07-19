#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int w = 0, h = 0;
unsigned char *pix;

void refresh(int x, int y)
{
	int i, j, k;
	printf("\033[H");
	for (i = k = 0; i < h; putchar('\n'), i++)
		for (j = 0; j < w; j++, k++)
			putchar(pix[k] ? '#' : ' ');
}

void walk()
{
	int dx = 0, dy = 1, i, k;
	int x = w / 2, y = h / 2;

	pix = calloc(1, w * h);
	printf("\033[H\033[J");

	while (1) {
		i = (y * w + x);
		if (pix[i]) k = dx, dx = -dy, dy = k;
		else	    k = dy, dy = -dx, dx = k;

		pix[i] = !pix[i];
		printf("\033[%d;%dH%c", y + 1, x + 1, pix[i] ? '#' : ' ');

		x += dx, y += dy;

		k = 0;
		if (x < 0) {
			memmove(pix + 1, pix, w * h - 1);
			for (i = 0; i < w * h; i += w) pix[i] = 0;
			x++, k = 1;
		}
		else if (x >= w) {
			memmove(pix, pix + 1, w * h - 1);
			for (i = w-1; i < w * h; i += w) pix[i] = 0;
			x--, k = 1;
		}

		if (y >= h) {
			memmove(pix, pix + w, w * (h - 1));
			memset(pix + w * (h - 1), 0, w);
			y--, k = 1;
		}
		else if (y < 0) {
			memmove(pix + w, pix, w * (h - 1));
			memset(pix, 0, w);
			y++, k = 1;
		}
		if (k) refresh(x, y);
		printf("\033[%d;%dH\033[31m@\033[m", y + 1, x + 1);

		fflush(stdout);
		usleep(10000);
	}
}

int main(int c, char **v)
{
	if (c > 1) w = atoi(v[1]);
	if (c > 2) h = atoi(v[2]);
	if (w < 40) w = 40;
	if (h < 25) h = 25;

	walk();
	return 0;
}
