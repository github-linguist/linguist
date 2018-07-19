#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/* BNFesque
	rangelist := (range | number) [',' rangelist]
	range := number '-' number	*/

int get_list(const char *, char **);
int get_rnge(const char *, char **);

/* parser only parses; what to do with parsed items is up to
* the add_number and and_range functions */
void add_number(int x);
int add_range(int x, int y);

#define skip_space while(isspace(*s)) s++
#define get_number(x, s, e) (x = strtol(s, e, 10), *e != s)
int get_list(const char *s, char **e)
{
	int x;
	while (1) {
		skip_space;
		if (!get_rnge(s, e) && !get_number(x, s, e)) break;
		s = *e;

		skip_space;
		if ((*s) == '\0') { putchar('\n'); return 1; }
		if ((*s) == ',')  { s++; continue; }
		break;
	}
	*(const char **)e = s;
	printf("\nSyntax error at %s\n", s);
	return 0;
}

int get_rnge(const char *s, char **e)
{
	int x, y;
	char *ee;
	if (!get_number(x, s, &ee)) return 0;
	s = ee;

	skip_space;
	if (*s != '-') {
		*(const char **)e = s;
		return 0;
	}
	s++;
	if(!get_number(y, s, e)) return 0;
	return add_range(x, y);
}

void add_number(int x)
{
	printf("%d ", x);
}

int add_range(int x, int y)
{
	if (y <= x) return 0;
	while (x <= y) printf("%d ", x++);
	return 1;
}

int main()
{
	char *end;

	/* this is correct */
	if (get_list("-6,-3--1,3-5,7-11,14,15,17-20", &end)) puts("Ok");

	/* this is not.  note the subtle error: "-6 -3" is parsed
	 * as range(-6, 3), so synax error comes after that */
	get_list("-6 -3--1,3-5,7-11,14,15,17-20", &end);

	return 0;
}
