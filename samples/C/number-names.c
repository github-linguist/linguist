#include <stdio.h>
#include <string.h>

const char *ones[] = { 0, "one", "two", "three", "four",
	"five", "six", "seven", "eight", "nine",
	"ten", "eleven", "twelve", "thirteen", "fourteen",
	"fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };
const char *tens[] = { 0, "ten", "twenty", "thirty", "forty",
	"fifty", "sixty", "seventy", "eighty", "ninety" };
const char *llions[] = { 0, "thousand", "million", "billion", "trillion",
//	"quadrillion", "quintillion", "sextillion", "septillion",
//	"octillion", "nonillion", "decillion"
	};
const int maxillion = sizeof(llions) / sizeof(llions[0]) * 3 - 3;

int say_hundred(const char *s, int len, int depth, int has_lead)
{
	int c[3], i;
	for (i = -3; i < 0; i++) {
		if (len + i >= 0) c[i + 3] = s[len + i] - '0';
		else c[i + 3] = 0;
	}
	if (!(c[0] + c[1] + c[2])) return 0;

	if (c[0]) {
		printf("%s hundred", ones[c[0]]);
		has_lead = 1;
	}

	if (has_lead && (c[1] || c[2]))
		printf((!depth || c[0]) && (!c[0] || !c[1]) ? "and " :
			c[0] ? " " : "");

	if (c[1] < 2) {
		if (c[1] || c[2]) printf("%s", ones[c[1] * 10 + c[2]]);
	} else {
		if (c[1]) {
			printf("%s", tens[c[1]]);
			if (c[2]) putchar('-');
		}
		if (c[2]) printf("%s", ones[c[2]]);
	}

	return 1;
}

int say_maxillion(const char *s, int len, int depth, int has_lead)
{
	int n = len / 3, r = len % 3;
	if (!r) {
		n--;
		r = 3;
	}
	const char *e = s + r;
	do {
		if (say_hundred(s, r, n, has_lead) && n) {
			has_lead = 1;
			printf(" %s", llions[n]);
			if (!depth) printf(", ");
			else printf(" ");
		}
		s = e; e += 3;
	} while (r = 3, n--);

	return 1;
}

void say_number(const char *s)
{
	int len, i, got_sign = 0;

	while (*s == ' ') s++;
	if (*s < '0' || *s > '9') {
		if (*s == '-') got_sign = -1;
		else if (*s == '+') got_sign = 1;
		else goto nan;
		s++;
	} else
		got_sign = 1;

	while (*s == '0') {
		s++;
		if (*s == '\0') {
			printf("zero\n");
			return;
		}
	}

	len = strlen(s);
	if (!len) goto nan;

	for (i = 0; i < len; i++) {
		if (s[i] < '0' || s[i] > '9') {
			printf("(not a number)");
			return;
		}
	}
	if (got_sign == -1) printf("minus ");

	int n = len / maxillion;
	int r = len % maxillion;
	if (!r) {
		r = maxillion;
		n--;
	}

	const char *end = s + len - n * maxillion;
	int has_lead = 0;
	do {
		if ((has_lead = say_maxillion(s, r, n, has_lead))) {
			for (i = 0; i < n; i++)
				printf(" %s", llions[maxillion / 3]);
			if (n) printf(", ");
		}
		n--;
		r = maxillion;
		s = end;
		end += r;
	} while (n >= 0);

	printf("\n");
	return;

nan:	printf("not a number\n");
	return;
}

int main()
{
	say_number("-42");
	say_number("1984");
	say_number("10000");
	say_number("1024");
	say_number("1001001001001");
	say_number("123456789012345678901234567890123456789012345678900000001");
	return 0;
}
