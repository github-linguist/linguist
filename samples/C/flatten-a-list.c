#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct list_t list_t, *list;
struct list_t{
	int is_list, ival; /* ival is either the integer value or list length */
	list *lst;
};

list new_list()
{
	list x = malloc(sizeof(list_t));
	x->ival = 0;
	x->is_list = 1;
	x->lst = 0;
	return x;
}

void append(list parent, list child)
{
	parent->lst = realloc(parent->lst, sizeof(list) * (parent->ival + 1));
	parent->lst[parent->ival++] = child;
}

list from_string(char *s, char **e, list parent)
{
	list ret = 0;
	if (!parent) parent = new_list();

	while (*s != '\0') {
		if (*s == ']') {
			if (e) *e = s + 1;
			return parent;
		}
		if (*s == '[') {
			ret = new_list();
			ret->is_list = 1;
			ret->ival = 0;
			append(parent, ret);
			from_string(s + 1, &s, ret);
			continue;
		}
		if (*s >= '0' && *s <= '9') {
			ret = new_list();
			ret->is_list = 0;
			ret->ival = strtol(s, &s, 10);
			append(parent, ret);
			continue;
		}
		s++;
	}

	if (e) *e = s;
	return parent;
}

void show_list(list l)
{
	int i;
	if (!l) return;
	if (!l->is_list) {
		printf("%d", l->ival);
		return;
	}

	printf("[");
	for (i = 0; i < l->ival; i++) {
		show_list(l->lst[i]);
		if (i < l->ival - 1) printf(", ");
	}
	printf("]");
}

list flatten(list from, list to)
{
	int i;
	list t;

	if (!to) to = new_list();
	if (!from->is_list) {
		t = new_list();
		*t = *from;
		append(to, t);
	} else
		for (i = 0; i < from->ival; i++)
			flatten(from->lst[i], to);
	return to;
}

void delete_list(list l)
{
	int i;
	if (!l) return;
	if (l->is_list && l->ival) {
		for (i = 0; i < l->ival; i++)
			delete_list(l->lst[i]);
		free(l->lst);
	}

	free(l);
}

int main()
{
	list l = from_string("[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []", 0, 0);

	printf("Nested: ");
	show_list(l);
	printf("\n");

	list flat = flatten(l, 0);
	printf("Flattened: ");
	show_list(flat);

	/* delete_list(l); delete_list(flat); */
	return 0;
}
