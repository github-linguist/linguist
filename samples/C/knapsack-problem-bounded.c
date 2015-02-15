#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define max_weight 400

typedef struct { const char *name; int w, v, qty; } item_t;

item_t items[] = {
	{"map",			9,	150,	1},
	{"compass",		13,	35,	1},
	{"water",		153,	200,	2},
	{"sandwich",		50,	60,	2},
	{"glucose",		15,	60,	2},
	{"tin",			68,	45,	3},
	{"banana",		27,	60,	3},
	{"apple",		39,	40,	3},
	{"cheese",		23,	30,	1},
	{"beer",		52,	10,	3},
	{"suntancream",		11,	70,	1},
	{"camera",		32,	30,	1},
	{"T-shirt",		24,	15,	2},
	{"trousers",		48,	10,	2},
	{"umbrella",		73,	40,	1},
	{"w-trousers",		42,	70,	1},
	{"w-overclothes",	43,	75,	1},
	{"note-case",		22,	80,	1},
	{"sunglasses",		7,      20,	1},
	{"towel",		18,	12,	2},
	{"socks",		4,      50,	1},
	{"book",		30,	10,	2},
};

/* for C, the main problem is not the algorithm: it's cache management */
#define n_types (sizeof(items)/sizeof(item_t))
typedef struct {
	int v, w;		   /* value & weight total */
	unsigned short n[n_types]; /* num of each item taken */
} solution_t, *solution;

solution_t *cache, *blank;

int init_cache()
{
	/* a flat array.  If problem size is large, might be a bad idea;
	 * then again, other caching method face similar issue, too
	 */
	size_t i;
	size_t n_rec = (max_weight + 1) * n_types;
	cache = calloc(sizeof(solution_t), (n_rec + 1));
	if (!cache) return 0;

	for (i = 0; i <= n_rec; i++) {
		cache[i].v = -1;  /* value = -1 means cache not used yet */
		cache[i].w = 0;
	}
	(blank = cache + n_rec)->v = 0;
	return 1;
}

solution solve(int weight, int pos)
{
	int i, w, v, qty;
	solution sol, best = 0, ret;

	if (pos < 0) return blank;

	ret = &cache[weight * n_types + pos];
	if (ret->v >= 0) return ret;

	w   = items[pos].w;
	v   = items[pos].v;
	qty = items[pos].qty;

	for (i = 0; i <= qty && weight >= 0; i++, weight -= w) {
		sol = solve(weight, pos - 1);
		if (sol->v + i * v <= ret->v) continue;

		best = sol;
		ret->v = best->v + v * i;
		ret->n[pos] = i;
	}

	/*  only happens if there are no solution at all, i.e.
	 *  max_weight too small to hold even one item
	 */
	if (ret->v <= 0) return blank;

	ret->w = best->w + w * ret->n[pos];
	memcpy(ret->n, best->n, sizeof(unsigned short) * pos);

	return ret;
}

int main()
{
	int i;
	solution x;

	init_cache();
	x = solve(max_weight, n_types - 1);

	printf("Taking:\n");
	for (i = 0; i < n_types; i++) {
		if (! x->n[i]) continue;
		printf("  %hu %s\n", x->n[i], items[i].name);
	}

	printf("Weight: %d Value: %d\n", x->w, x->v);

	/* free(cache); */
	return 0;
}
