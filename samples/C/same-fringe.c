#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>

typedef struct {
	ucontext_t caller, callee;
	char stack[8192];
	void *in, *out;
} co_t;

co_t * co_new(void(*f)(), void *data)
{
	co_t * c = malloc(sizeof(*c));
	getcontext(&c->callee);
	c->in = data;

	c->callee.uc_stack.ss_sp = c->stack;
	c->callee.uc_stack.ss_size = sizeof(c->stack);
	c->callee.uc_link = &c->caller;
	makecontext(&c->callee, f, 1, (int)c);

	return c;
}

void co_del(co_t *c)
{
	free(c);
}

inline void
co_yield(co_t *c, void *data)
{
	c->out = data;
	swapcontext(&c->callee, &c->caller);
}

inline void *
co_collect(co_t *c)
{
	c->out = 0;
	swapcontext(&c->caller, &c->callee);
	return c->out;
}

// end of coroutine stuff

typedef struct node node;
struct node {
	int v;
	node *left, *right;
};

node *newnode(int v)
{
	node *n = malloc(sizeof(node));
	n->left = n->right = 0;
	n->v = v;
	return n;
}

void tree_insert(node **root, node *n)
{
	while (*root) root = ((*root)->v > n->v)
				? &(*root)->left
				: &(*root)->right;
	*root = n;
}

void tree_trav(int x)
{
	co_t *c = (co_t *) x;

	void trav(node *root) {
		if (!root) return;
		trav(root->left);
		co_yield(c, root);
		trav(root->right);
	}

	trav(c->in);
}

int tree_eq(node *t1, node *t2)
{
	co_t *c1 = co_new(tree_trav, t1);
	co_t *c2 = co_new(tree_trav, t2);

	node *p = 0, *q = 0;
	do {
		p = co_collect(c1);
		q = co_collect(c2);
	} while (p && q && (p->v == q->v));

	co_del(c1);
	co_del(c2);
	return !p && !q;
}

int main()
{
	int x[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1 };
	int y[] = { 2, 5, 7, 1, 9, 0, 6, 4, 8, 3, -1 };
	int z[] = { 0, 1, 2, 3, 4, 5, 6, 8, 9, -1 };

	node *t1 = 0, *t2 = 0, *t3 = 0;

	void mktree(int *buf, node **root) {
		int i;
		for (i = 0; buf[i] >= 0; i++)
			tree_insert(root, newnode(buf[i]));
	}

	mktree(x, &t1); // ordered binary tree, result of traversing
	mktree(y, &t2); // should be independent of insertion, so t1 == t2
	mktree(z, &t3);

	printf("t1 == t2: %s\n", tree_eq(t1, t2) ? "yes" : "no");
	printf("t1 == t3: %s\n", tree_eq(t1, t3) ? "yes" : "no");

	return 0;
}
