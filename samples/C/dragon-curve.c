#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* x, y: coordinates of current point; dx, dy: direction of movement.
 * Think turtle graphics.  They are divided by scale, so as to keep
 * very small coords/increments without losing precission. clen is
 * the path length travelled, which should equal to scale at the end
 * of the curve.
 */
long long x, y, dx, dy, scale, clen;
typedef struct { double r, g, b; } rgb;
rgb ** pix;

/* for every depth increase, rotate 45 degrees and scale up by sqrt(2)
 * Note how coords can still be represented by integers.
 */
void sc_up()
{
	long long tmp = dx - dy; dy = dx + dy; dx = tmp;
	scale *= 2; x *= 2; y *= 2;
}

/* Hue changes from 0 to 360 degrees over entire length of path; Value
 * oscillates along the path to give some contrast between segments
 * close to each other spatially.  RGB derived from HSV gets *added*
 * to each pixel reached; they'll be dealt with later.
 */
void h_rgb(long long x, long long y)
{
	rgb *p = &pix[y][x];

#	define SAT 1
	double h = 6.0 * clen / scale;
	double VAL = 1 - (cos(3.141592653579 * 64 * clen / scale) - 1) / 4;
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

/* string rewriting.  No need to keep the string itself, just execute
 * its instruction recursively.
 */
void iter_string(const char * str, int d)
{
	long tmp;
#	define LEFT  tmp = -dy; dy = dx; dx = tmp
#	define RIGHT tmp = dy; dy = -dx; dx = tmp
	while (*str != '\0') {
		switch(*(str++)) {
		case 'X':	if (d) iter_string("X+YF+", d - 1); continue;
		case 'Y':	if (d) iter_string("-FX-Y", d - 1); continue;
		case '+':	RIGHT; continue;
		case '-':	LEFT;  continue;
		case 'F':
                        /* draw: increment path length; add color; move. Here
                         * is why the code does not allow user to choose arbitrary
                         * image size: if it's not a power of two, aliasing will
                         * occur and grid-like bright or dark lines will result
                         * when normalized later.  It can be gotten rid of, but that
                         * involves computing multiplicative order and would be a huge
                         * bore.
                         */
				clen ++;
				h_rgb(x/scale, y/scale);
				x += dx; y += dy;
				continue;
		}
	}
}

void dragon(long leng, int depth)
{
	long i, d = leng / 3 + 1;
	long h = leng + 3, w = leng + d * 3 / 2 + 2;

	/* allocate pixel buffer */
	rgb *buf = malloc(sizeof(rgb) * w * h);
	pix = malloc(sizeof(rgb *) * h);
	for (i = 0; i < h; i++)
		pix[i] = buf + w * i;
	memset(buf, 0, sizeof(rgb) * w * h);

        /* init coords; scale up to desired; exec string */
	x = y = d; dx = leng; dy = 0; scale = 1; clen = 0;
	for (i = 0; i < depth; i++) sc_up();
	iter_string("FX", depth);

	/* write color PNM file */
	unsigned char *fpix = malloc(w * h * 3);
	double maxv = 0, *dbuf = (double*)buf;

        /* find highest value among pixels; normalize image according
         * to it.  Highest value would be at points most travelled, so
         * this ends up giving curve edge a nice fade -- it's more apparaent
         * if we increase iteration depth by one or two.
         */
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
	dragon(size, depth * 2);

	return 0;
}
