#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N_SITES 150
double site[N_SITES][2];
unsigned char rgb[N_SITES][3];

int size_x = 640, size_y = 480;

inline double sq2(double x, double y)
{
	return x * x + y * y;
}

#define for_k for (k = 0; k < N_SITES; k++)
int nearest_site(double x, double y)
{
	int k, ret = 0;
	double d, dist = 0;
	for_k {
		d = sq2(x - site[k][0], y - site[k][1]);
		if (!k || d < dist) {
			dist = d, ret = k;
		}
	}
	return ret;
}

/* see if a pixel is different from any neighboring ones */
int at_edge(int *color, int y, int x)
{
	int i, j, c = color[y * size_x + x];
	for (i = y - 1; i <= y + 1; i++) {
		if (i < 0 || i >= size_y) continue;

		for (j = x - 1; j <= x + 1; j++) {
			if (j < 0 || j >= size_x) continue;
			if (color[i * size_x + j] != c) return 1;
		}
	}
	return 0;
}

#define AA_RES 4 /* average over 4x4 supersampling grid */
void aa_color(unsigned char *pix, int y, int x)
{
	int i, j, n;
	double r = 0, g = 0, b = 0, xx, yy;
	for (i = 0; i < AA_RES; i++) {
		yy = y + 1. / AA_RES * i + .5;
		for (j = 0; j < AA_RES; j++) {
			xx = x + 1. / AA_RES * j + .5;
			n = nearest_site(xx, yy);
			r += rgb[n][0];
			g += rgb[n][1];
			b += rgb[n][2];
		}
	}
	pix[0] = r / (AA_RES * AA_RES);
	pix[1] = g / (AA_RES * AA_RES);
	pix[2] = b / (AA_RES * AA_RES);
}

#define for_i for (i = 0; i < size_y; i++)
#define for_j for (j = 0; j < size_x; j++)
void gen_map()
{
	int i, j, k;
	int *nearest = malloc(sizeof(int) * size_y * size_x);
	unsigned char *ptr, *buf, color;

	ptr = buf = malloc(3 * size_x * size_y);
	for_i for_j nearest[i * size_x + j] = nearest_site(j, i);

	for_i for_j {
		if (!at_edge(nearest, i, j))
			memcpy(ptr, rgb[nearest[i * size_x + j]], 3);
		else	/* at edge, do anti-alias rastering */
			aa_color(ptr, i, j);
		ptr += 3;
	}

	/* draw sites */
	for (k = 0; k < N_SITES; k++) {
		color = (rgb[k][0]*.25 + rgb[k][1]*.6 + rgb[k][2]*.15 > 80) ? 0 : 255;

		for (i = site[k][1] - 1; i <= site[k][1] + 1; i++) {
			if (i < 0 || i >= size_y) continue;

			for (j = site[k][0] - 1; j <= site[k][0] + 1; j++) {
				if (j < 0 || j >= size_x) continue;

				ptr = buf + 3 * (i * size_x + j);
				ptr[0] = ptr[1] = ptr[2] = color;
			}
		}
	}

	printf("P6\n%d %d\n255\n", size_x, size_y);
	fflush(stdout);
	fwrite(buf, size_y * size_x * 3, 1, stdout);
}

#define frand(x) (rand() / (1. + RAND_MAX) * x)
int main()
{
	int k;
	for_k {
		site[k][0] = frand(size_x);
		site[k][1] = frand(size_y);
		rgb [k][0] = frand(256);
		rgb [k][1] = frand(256);
		rgb [k][2] = frand(256);
	}

	gen_map();
	return 0;
}
