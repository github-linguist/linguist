#include <stdio.h>

typedef unsigned long long u64;

#define FIB_INVALID (~(u64)0)

u64 fib[] = {
	1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,
	2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418,
	317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465,
	14930352, 24157817, 39088169, 63245986, 102334155, 165580141,
	267914296, 433494437, 701408733, 1134903170, 1836311903,
	2971215073ULL, 4807526976ULL, 7778742049ULL, 12586269025ULL,
	20365011074ULL, 32951280099ULL, 53316291173ULL, 86267571272ULL,
	139583862445ULL, 225851433717ULL, 365435296162ULL, 591286729879ULL,
	956722026041ULL, 1548008755920ULL, 2504730781961ULL, 4052739537881ULL,
	6557470319842ULL, 10610209857723ULL, 17167680177565ULL,

	27777890035288ULL // this 65-th one is for range check
};

u64 fibbinary(u64 n)
{
	if (n >= fib[64]) return FIB_INVALID;

	u64 ret = 0;
	int i;
	for (i = 64; i--; )
		if (n >= fib[i]) {
			ret |= 1ULL << i;
			n -= fib[i];
		}

	return ret;
}

void bprint(u64 n, int width)
{
	if (width > 64) width = 64;

	u64 b;
	for (b = 1ULL << (width - 1); b; b >>= 1)
		putchar(b == 1 && !n
			? '0'
			: b > n	? ' '
				: b & n ? '1' : '0');
	putchar('\n');
}

int main(void)
{
	int i;

	for (i = 0; i <= 20; i++)
		printf("%2d:", i), bprint(fibbinary(i), 8);

	return 0;
}
