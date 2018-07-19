#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* for ASCII only */
static char code[128] = { 0 };
void add_code(const char *s, int c)
{
	while (*s) {
		code[(int)*s] = code[0x20 ^ (int)*s] = c;
		s++;
	}
}

void init()
{
	static const char *cls[] =
		{ "AEIOU", "", "BFPV", "CGJKQSXZ", "DT", "L", "MN", "R", 0};
	int i;
	for (i = 0; cls[i]; i++)
		add_code(cls[i], i - 1);
}

/* returns a static buffer; user must copy if want to save
   result across calls */
const char* soundex(const char *s)
{
	static char out[5];
	int c, prev, i;

	out[0] = out[4] = 0;
	if (!s || !*s) return out;

	out[0] = *s++;

	/* first letter, though not coded, can still affect next letter: Pfister */
	prev = code[(int)out[0]];
	for (i = 1; *s && i < 4; s++) {
		if ((c = code[(int)*s]) == prev) continue;

		if (c == -1) prev = 0;	/* vowel as separator */
		else if (c > 0) {
			out[i++] = c + '0';
			prev = c;
		}
	}
	while (i < 4) out[i++] = '0';
	return out;
}

int main()
{
	int i;
	const char *sdx, *names[][2] = {
		{"Soundex",	"S532"},
		{"Example",	"E251"},
		{"Sownteks",	"S532"},
		{"Ekzampul",	"E251"},
		{"Euler",	"E460"},
		{"Gauss",	"G200"},
		{"Hilbert",	"H416"},
		{"Knuth",	"K530"},
		{"Lloyd",	"L300"},
		{"Lukasiewicz",	"L222"},
		{"Ellery",	"E460"},
		{"Ghosh",	"G200"},
		{"Heilbronn",	"H416"},
		{"Kant",	"K530"},
		{"Ladd",	"L300"},
		{"Lissajous",	"L222"},
		{"Wheaton",	"W350"},
		{"Burroughs",	"B620"},
		{"Burrows",	"B620"},
		{"O'Hara",	"O600"},
		{"Washington",	"W252"},
		{"Lee",		"L000"},
		{"Gutierrez",	"G362"},
		{"Pfister",	"P236"},
		{"Jackson",	"J250"},
		{"Tymczak",	"T522"},
		{"VanDeusen",	"V532"},
		{"Ashcraft",	"A261"},
		{0, 0}
	};

	init();

	puts("  Test name  Code  Got\n----------------------");
	for (i = 0; names[i][0]; i++) {
		sdx = soundex(names[i][0]);
		printf("%11s  %s  %s ", names[i][0], names[i][1], sdx);
		printf("%s\n", strcmp(sdx, names[i][1]) ? "not ok" : "ok");
	}

	return 0;
}
