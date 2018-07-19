#include <stdio.h>

typedef unsigned int set_t; /* probably 32 bits; change according to need */

void show_set(set_t x, const char *name)
{
	int i;
	printf("%s is:", name);
	for (i = 0; (1U << i) <= x; i++)
		if (x & (1U << i))
			printf(" %d", i);
	putchar('\n');
}

int main(void)
{
	int i;
	set_t a, b, c;
	
	a = 0; /* empty set */
	for (i = 0; i < 10; i += 3) /* add 0 3 6 9 to set a */
		a |= (1U << i);
	show_set(a, "a");

	for (i = 0; i < 5; i++)
		printf("\t%d%s in set a\n", i, (a & (1U << i)) ? "":" not");

	b = a;
	b |= (1U << 5); b |= (1U << 10); /* b is a plus 5, 10 */
	b &= ~(1U << 0);	/* sans 0 */
	show_set(b, "b");

	show_set(a | b, "union(a, b)");
	show_set(c = a & b, "c = common(a, b)");
	show_set(a & ~b, "a - b"); /* diff, not arithmetic minus */
	show_set(b & ~a, "b - a");
	printf("b is%s a subset of a\n", !(b & ~a) ? "" : " not");
	printf("c is%s a subset of a\n", !(c & ~a) ? "" : " not");

	printf("union(a, b) - common(a, b) %s union(a - b, b - a)\n",
		((a | b) & ~(a & b)) == ((a & ~b) | (b & ~a))
			? "equals" : "does not equal");

	return 0;
}
