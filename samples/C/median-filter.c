#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>

typedef struct { unsigned char r, g, b; } rgb_t;
typedef struct {
	int w, h;
	rgb_t **pix;
} image_t, *image;

typedef struct {
	int r[256], g[256], b[256];
	int n;
} color_histo_t;

int write_ppm(image im, char *fn)
{
	FILE *fp = fopen(fn, "w");
	if (!fp) return 0;
	fprintf(fp, "P6\n%d %d\n255\n", im->w, im->h);
	fwrite(im->pix[0], 1, sizeof(rgb_t) * im->w * im->h, fp);
	fclose(fp);
	return 1;
}

image img_new(int w, int h)
{
	int i;
	image im = malloc(sizeof(image_t) + h * sizeof(rgb_t*)
			+ sizeof(rgb_t) * w * h);
	im->w = w; im->h = h;
	im->pix = (rgb_t**)(im + 1);
	for (im->pix[0] = (rgb_t*)(im->pix + h), i = 1; i < h; i++)
		im->pix[i] = im->pix[i - 1] + w;
	return im;
}

int read_num(FILE *f)
{
	int n;
	while (!fscanf(f, "%d ", &n)) {
		if ((n = fgetc(f)) == '#') {
			while ((n = fgetc(f)) != '\n')
				if (n == EOF) break;
			if (n == '\n') continue;
		} else return 0;
	}
	return n;
}

image read_ppm(char *fn)
{
	FILE *fp = fopen(fn, "r");
	int w, h, maxval;
	image im = 0;
	if (!fp) return 0;

	if (fgetc(fp) != 'P' || fgetc(fp) != '6' || !isspace(fgetc(fp)))
		goto bail;

	w = read_num(fp);
	h = read_num(fp);
	maxval = read_num(fp);
	if (!w || !h || !maxval) goto bail;

	im = img_new(w, h);
	fread(im->pix[0], 1, sizeof(rgb_t) * w * h, fp);
bail:
	if (fp) fclose(fp);
	return im;
}

void del_pixels(image im, int row, int col, int size, color_histo_t *h)
{
	int i;
	rgb_t *pix;

	if (col < 0 || col >= im->w) return;
	for (i = row - size; i <= row + size && i < im->h; i++) {
		if (i < 0) continue;
		pix = im->pix[i] + col;
		h->r[pix->r]--;
		h->g[pix->g]--;
		h->b[pix->b]--;
		h->n--;
	}
}

void add_pixels(image im, int row, int col, int size, color_histo_t *h)
{
	int i;
	rgb_t *pix;

	if (col < 0 || col >= im->w) return;
	for (i = row - size; i <= row + size && i < im->h; i++) {
		if (i < 0) continue;
		pix = im->pix[i] + col;
		h->r[pix->r]++;
		h->g[pix->g]++;
		h->b[pix->b]++;
		h->n++;
	}
}

void init_histo(image im, int row, int size, color_histo_t*h)
{
	int j;

	memset(h, 0, sizeof(color_histo_t));

	for (j = 0; j < size && j < im->w; j++)
		add_pixels(im, row, j, size, h);
}

int median(const int *x, int n)
{
	int i;
	for (n /= 2, i = 0; i < 256 && (n -= x[i]) > 0; i++);
	return i;
}

void median_color(rgb_t *pix, const color_histo_t *h)
{
	pix->r = median(h->r, h->n);
	pix->g = median(h->g, h->n);
	pix->b = median(h->b, h->n);
}

image median_filter(image in, int size)
{
	int row, col;
	image out = img_new(in->w, in->h);
	color_histo_t h;

	for (row = 0; row < in->h; row ++) {
		for (col = 0; col < in->w; col++) {
			if (!col) init_histo(in, row, size, &h);
			else {
				del_pixels(in, row, col - size, size, &h);
				add_pixels(in, row, col + size, size, &h);
			}
			median_color(out->pix[row] + col, &h);
		}
	}

	return out;
}

int main(int c, char **v)
{
	int size;
	image in, out;
	if (c <= 3) {
		printf("Usage: %s size ppm_in ppm_out\n", v[0]);
		return 0;
	}
	size = atoi(v[1]);
	printf("filter size %d\n", size);
	if (size < 0) size = 1;

	in = read_ppm(v[2]);
	out = median_filter(in, size);
	write_ppm(out, v[3]);
	free(in);
	free(out);

	return 0;
}
