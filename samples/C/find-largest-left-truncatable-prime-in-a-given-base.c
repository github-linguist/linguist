#include <stdio.h>
#include <gmp.h>

typedef unsigned long ulong;

ulong small_primes[] = {2,3,5,7,11,13,17,19,23,29,31,37,41,
			43,47,53,59,61,67,71,73,79,83,89,97};

#define MAX_STACK 128
mpz_t tens[MAX_STACK], value[MAX_STACK], answer;

ulong base, seen_depth;

void add_digit(ulong i)
{
	ulong d;
	for (d = 1; d < base; d++) {
		mpz_set(value[i], value[i-1]);
		mpz_addmul_ui(value[i], tens[i], d);
		if (!mpz_probab_prime_p(value[i], 1)) continue;

		if (i > seen_depth ||
			(i == seen_depth && mpz_cmp(value[i], answer) == 1))
		{
			if (!mpz_probab_prime_p(value[i], 50)) continue;

			mpz_set(answer, value[i]);
			seen_depth = i;
			gmp_fprintf(stderr, "\tb=%lu d=%2lu | %Zd\n", base, i, answer);
		}

		add_digit(i+1);
	}
}

void do_base()
{
	ulong i;
	mpz_set_ui(answer, 0);
	mpz_set_ui(tens[0], 1);
	for (i = 1; i < MAX_STACK; i++)
		mpz_mul_ui(tens[i], tens[i-1], base);

	for (seen_depth = i = 0; small_primes[i] < base; i++) {
		fprintf(stderr, "\tb=%lu digit %lu\n", base, small_primes[i]);
		mpz_set_ui(value[0], small_primes[i]);
		add_digit(1);
	}
	gmp_printf("%d: %Zd\n", base, answer);
}

int main(void)
{
	ulong i;
	for (i = 0; i < MAX_STACK; i++) {
		mpz_init_set_ui(tens[i], 0);
		mpz_init_set_ui(value[i], 0);
	}
	mpz_init_set_ui(answer, 0);

	for (base = 22; base < 30; base++) do_base();

	return 0;
}
