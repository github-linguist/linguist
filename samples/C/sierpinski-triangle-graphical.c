#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

long long x, y, dx, dy, scale, clen, cscale;
typedef struct { double r, g, b; } rgb;
rgb ** pix;

void sc_up()
{
	scale *= 2; x *= 2; y *= 2;
	cscale *= 3;
}

void h_rgb(long long x, long long y)
{
	rgb *p = &pix[y][x];

#	define SAT 1
	double h = 6.0 * clen / cscale;
	double VAL = 1;
	double c = SAT * VAL;
	double X = c * (1 - fabs(fmod(h, 2) - 1));

	switch((int)h) {
	case 0: p->r += c; p->g += X; return;
	case 1:	p->r += X; p->g += c; return;
	case 2: p->g += c; p->b += X; return;
	case 3: p->g += X; p->b += c; return;
	case 4: p->r += X; p->b += c; return;
	default:
		p->r += c; p->b += X;
	}
}

void iter_string(const char * str, int d)
{
	long long len;
	while (*str != '\0') {
		switch(*(str++)) {
		case 'X':
			if (d)	iter_string("XHXVX", d - 1);
			else{
				clen ++;
				h_rgb(x/scale, y/scale);
				x += dx;
				y -= dy;
			}
			continue;
		case 'V':
			len = 1LLU << d;
			while (len--) {
				clen ++;
				h_rgb(x/scale, y/scale);
				y += dy;
			}
			continue;
		case 'H':
			len = 1LLU << d;
			while(len --) {
				clen ++;
				h_rgb(x/scale, y/scale);
				x -= dx;
			}
			continue;
		}
	}
}

void sierp(long leng, int depth)
{
	long i;
	long h = leng + 20, w = leng + 20;

	/* allocate pixel buffer */
	rgb *buf = malloc(sizeof(rgb) * w * h);
	pix = malloc(sizeof(rgb *) * h);
	for (i = 0; i < h; i++)
		pix[i] = buf + w * i;
	memset(buf, 0, sizeof(rgb) * w * h);

        /* init coords; scale up to desired; exec string */
	x = y = 10; dx = leng; dy = leng; scale = 1; clen = 0; cscale = 3;
	for (i = 0; i < depth; i++) sc_up();
	iter_string("VXH", depth);

	/* write color PNM file */
	unsigned char *fpix = malloc(w * h * 3);
	double maxv = 0, *dbuf = (double*)buf;

	for (i = 3 * w * h - 1; i >= 0; i--)
		if (dbuf[i] > maxv) maxv = dbuf[i];
	for (i = 3 * h * w - 1; i >= 0; i--)
		fpix[i] = 255 * dbuf[i] / maxv;

	printf("P6\n%ld %ld\n255\n", w, h);
	fflush(stdout); /* printf and fwrite may treat buffer differently */
	fwrite(fpix, h * w * 3, 1, stdout);
}

int main(int c, char ** v)
{
	int size, depth;

	depth  = (c > 1) ? atoi(v[1]) : 10;
	size = 1 << depth;

	fprintf(stderr, "size: %d depth: %d\n", size, depth);
	sierp(size, depth + 2);

	return 0;
}
