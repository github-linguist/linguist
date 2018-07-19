#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

char *to_base(int64_t num, int base)
{
	char *tbl = "0123456789abcdefghijklmnopqrstuvwxyz";
	char buf[66] = {'\0'};
	char *out;
	uint64_t n;
	int i, len = 0, neg = 0;
	if (base > 36) {
		fprintf(stderr, "base %d too large\n", base);
		return 0;
	}

	/* safe against most negative integer */
	n = ((neg = num < 0)) ? (~num) + 1 : num;

	do { buf[len++] = tbl[n % base]; } while(n /= base);

	out = malloc(len + neg + 1);
	for (i = neg; len > 0; i++) out[i] = buf[--len];
	if (neg) out[0] = '-';

	return out;
}

long from_base(const char *num_str, int base)
{
	char *endptr;
	/* there is also strtoul() for parsing into an unsigned long */
	/* in C99, there is also strtoll() and strtoull() for parsing into long long and
	 * unsigned long long, respectively */
	int result = strtol(num_str, &endptr, base);
	return result;
}

int main()
{
	int64_t x;
	x = ~(1LL << 63) + 1;
	printf("%lld in base 2: %s\n", x, to_base(x, 2));
	x = 383;
	printf("%lld in base 16: %s\n", x, to_base(x, 16));
	return 0;
}
