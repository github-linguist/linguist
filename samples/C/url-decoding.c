#include <stdio.h>
#include <string.h>

inline int ishex(int x)
{
	return	(x >= '0' && x <= '9')	||
		(x >= 'a' && x <= 'f')	||
		(x >= 'A' && x <= 'F');
}

int decode(const char *s, char *dec)
{
	char *o;
	const char *end = s + strlen(s);
	int c;

	for (o = dec; s <= end; o++) {
		c = *s++;
		if (c == '+') c = ' ';
		else if (c == '%' && (	!ishex(*s++)	||
					!ishex(*s++)	||
					!sscanf(s - 2, "%2x", &c)))
			return -1;

		if (dec) *o = c;
	}

	return o - dec;
}

int main()
{
	const char *url = "http%3A%2F%2ffoo+bar%2fabcd";
	char out[sizeof(url)];

	printf("length: %d\n", decode(url, 0));
	puts(decode(url, out) < 0 ? "bad string" : out);

	return 0;
}
