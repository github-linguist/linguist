#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct { double x, y; } vec;
typedef struct { int n; vec* v; } polygon_t, *polygon;

#define BIN_V(op, xx, yy) vec v##op(vec a,vec b){vec c;c.x=xx;c.y=yy;return c;}
#define BIN_S(op, r) double v##op(vec a, vec b){ return r; }
BIN_V(sub, a.x - b.x, a.y - b.y);
BIN_V(add, a.x + b.x, a.y + b.y);
BIN_S(dot, a.x * b.x + a.y * b.y);
BIN_S(cross, a.x * b.y - a.y * b.x);

/* return a + s * b */
vec vmadd(vec a, double s, vec b)
{
	vec c;
	c.x = a.x + s * b.x;
	c.y = a.y + s * b.y;
	return c;
}

/* check if x0->x1 edge crosses y0->y1 edge. dx = x1 - x0, dy = y1 - y0, then
   solve  x0 + a * dx == y0 + b * dy with a, b in real
   cross both sides with dx, then: (remember, cross product is a scalar)
	x0 X dx = y0 X dx + b * (dy X dx)
   similarly,
	x0 X dy + a * (dx X dy) == y0 X dy
   there is an intersection iff 0 <= a <= 1 and 0 <= b <= 1

   returns: 1 for intersect, -1 for not, 0 for hard to say (if the intersect
   point is too close to y0 or y1)
*/
int intersect(vec x0, vec x1, vec y0, vec y1, double tol, vec *sect)
{
	vec dx = vsub(x1, x0), dy = vsub(y1, y0);
	double d = vcross(dy, dx), a;
	if (!d) return 0; /* edges are parallel */

	a = (vcross(x0, dx) - vcross(y0, dx)) / d;
	if (sect)
		*sect = vmadd(y0, a, dy);

	if (a < -tol || a > 1 + tol) return -1;
	if (a < tol || a > 1 - tol) return 0;

	a = (vcross(x0, dy) - vcross(y0, dy)) / d;
	if (a < 0 || a > 1) return -1;

	return 1;
}

/* distance between x and nearest point on y0->y1 segment.  if the point
   lies outside the segment, returns infinity */
double dist(vec x, vec y0, vec y1, double tol)
{
	vec dy = vsub(y1, y0);
	vec x1, s;
	int r;

	x1.x = x.x + dy.y; x1.y = x.y - dy.x;
	r = intersect(x, x1, y0, y1, tol, &s);
	if (r == -1) return HUGE_VAL;
	s = vsub(s, x);
	return sqrt(vdot(s, s));
}

#define for_v(i, z, p) for(i = 0, z = p->v; i < p->n; i++, z++)
/* returns 1 for inside, -1 for outside, 0 for on edge */
int inside(vec v, polygon p, double tol)
{
	/* should assert p->n > 1 */
	int i, k, crosses;
	vec *pv;
	double min_x, max_x, min_y, max_y;

	for (i = 0; i < p->n; i++) {
		k = i + 1 % p->n;
		min_x = dist(v, p->v[i], p->v[k], tol);
		if (min_x < tol) return 0;
	}

	min_x = max_x = p->v[0].x;
	min_y = max_y = p->v[1].y;

	/* calculate extent of polygon */
	for_v(i, pv, p) {
		if (pv->x > max_x) max_x = pv->x;
		if (pv->x < min_x) min_x = pv->x;
		if (pv->y > max_y) max_y = pv->y;
		if (pv->y < min_y) min_y = pv->y;
	}
	if (v.x < min_x || v.x > max_x || v.y < min_y || v.y > max_y)
		return -1;

	max_x -= min_x; max_x *= 2;
	max_y -= min_y; max_y *= 2;
	max_x += max_y;

	vec e;
	while (1) {
		crosses = 0;
		/* pick a rand point far enough to be outside polygon */
		e.x = v.x + (1 + rand() / (RAND_MAX + 1.)) * max_x;
		e.y = v.y + (1 + rand() / (RAND_MAX + 1.)) * max_x;

		for (i = 0; i < p->n; i++) {
			k = (i + 1) % p->n;
			k = intersect(v, e, p->v[i], p->v[k], tol, 0);

			/* picked a bad point, ray got too close to vertex.
			   re-pick */
			if (!k) break;

			if (k == 1) crosses++;
		}
		if (i == p->n) break;
	}
	return (crosses & 1) ? 1 : -1;
}

int main()
{
	vec vsq[] = {	{0,0}, {10,0}, {10,10}, {0,10},
			{2.5,2.5}, {7.5,0.1}, {7.5,7.5}, {2.5,7.5}};

	polygon_t sq = { 4, vsq }, /* outer square */
		sq_hole = { 8, vsq }; /* outer and inner square, ie hole */

	vec c = { 10, 5 }; /* on edge */
	vec d = { 5, 5 };

	printf("%d\n", inside(c, &sq, 1e-10));
	printf("%d\n", inside(c, &sq_hole, 1e-10));

	printf("%d\n", inside(d, &sq, 1e-10));	/* in */
	printf("%d\n", inside(d, &sq_hole, 1e-10));  /* out (in the hole) */

	return 0;
}
