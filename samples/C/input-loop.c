#include <stdlib.h>
#include <stdio.h>

char *get_line(FILE* fp)
{
	int len = 0, got = 0, c;
	char *buf = 0;

	while ((c = fgetc(fp)) != EOF) {
		if (got + 1 >= len) {
			len *= 2;
			if (len < 4) len = 4;
			buf = realloc(buf, len);
		}
		buf[got++] = c;
		if (c == '\n') break;
	}
	if (c == EOF && !got) return 0;

	buf[got++] = '\0';
	return buf;
}

int main()
{
	char *s;
	while ((s = get_line(stdin))) {
		printf("%s",s);
		free(s);
	}
	return 0;
}
