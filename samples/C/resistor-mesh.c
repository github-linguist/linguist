#include <stdio.h>
#include <stdlib.h>

#define S 10
typedef struct { double v; int fixed; } node;

#define each(i, x) for(i = 0; i < x; i++)
node **alloc2(int w, int h)
{
	int i;
	node **a = calloc(1, sizeof(node*)*h + sizeof(node)*w*h);
	each(i, h) a[i] = i ? a[i-1] + w : (node*)(a + h);
	return a;
}

void set_boundary(node **m)
{
	m[1][1].fixed =  1; m[1][1].v =  1;
	m[6][7].fixed = -1; m[6][7].v = -1;
}

double calc_diff(node **m, node **d, int w, int h)
{
	int i, j, n;
	double v, total = 0;
	each(i, h) each(j, w) {
		v = 0; n = 0;
		if (i) v += m[i-1][j].v, n++;
		if (j) v += m[i][j-1].v, n++;
		if (i+1 < h) v += m[i+1][j].v, n++;
		if (j+1 < w) v += m[i][j+1].v, n++;

		d[i][j].v = v = m[i][j].v - v / n;
		if (!m[i][j].fixed) total += v * v;
	}
	return total;
}

double iter(node **m, int w, int h)
{
	node **d = alloc2(w, h);
	int i, j;
	double diff = 1e10;
	double cur[] = {0, 0, 0};

	while (diff > 1e-24) {
		set_boundary(m);
		diff = calc_diff(m, d, w, h);
		each(i,h) each(j, w) m[i][j].v -= d[i][j].v;
	}

	each(i, h) each(j, w)
		cur[ m[i][j].fixed + 1 ] += d[i][j].v *
				(!!i + !!j + (i < h-1) + (j < w -1));

	free(d);
	return (cur[2] - cur[0])/2;
}

int main()
{
	node **mesh = alloc2(S, S);
	printf("R = %g\n", 2 / iter(mesh, S, S));
	return 0;
}
