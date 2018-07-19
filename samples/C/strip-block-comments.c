#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char *ca = "/*", *cb = "*/";
int al = 2, bl = 2;

char *loadfile(const char *fn) {
    FILE *f = fopen(fn, "rb");
    int l;
    char *s;

    if (f != NULL) {
	fseek(f, 0, SEEK_END);
	l = ftell(f);
	s = malloc(l+1);
	rewind(f);
	if (s)
	    fread(s, 1, l, f);
	fclose(f);
    }
    return s;
}

void stripcomments(char *s) {
    char *a, *b;
    int len = strlen(s) + 1;

    while ((a = strstr(s, ca)) != NULL) {
	b = strstr(a+al, cb);
	if (b == NULL)
	    break;
	b += bl;
	memmove(a, b, len-(b-a));
    }
}

int main(int argc, char **argv) {
    const char *fn = "input.txt";
    char *s;

    if (argc >= 2)
	fn = argv[1];
    s = loadfile(fn);
    if (argc == 4) {
	al = strlen(ca = argv[2]);
	bl = strlen(cb = argv[3]);
    }
    stripcomments(s);
    puts(s);
    free(s);
    return 0;
}
