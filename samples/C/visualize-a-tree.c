#include <stdio.h>
#include <stdlib.h>

typedef struct stem_t *stem;
struct stem_t { const char *str; stem next; };

void tree(int root, stem head)
{
	static const char *sdown = "  |", *slast = "  `", *snone = "   ";
	struct stem_t col = {0, 0}, *tail;

	for (tail = head; tail; tail = tail->next) {
		printf("%s", tail->str);
		if (!tail->next) break;
	}

	printf("--%d\n", root);

	if (root <= 1) return;

	if (tail && tail->str == slast)
		tail->str = snone;

	if (!tail)	tail = head = &col;
	else		tail->next = &col;

	while (root) { // make a tree by doing something random
		int r = 1 + (rand() % root);
		root -= r;
		col.str = root ? sdown : slast;

		tree(r, head);
	}

	tail->next = 0;
}

int main(int c, char**v)
{
	int n;
	if (c < 2 || (n = atoi(v[1])) < 0) n = 8;

	tree(n, 0);
	return 0;
}
