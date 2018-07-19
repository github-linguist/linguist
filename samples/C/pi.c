#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

mpz_t tmp1, tmp2, t5, t239, pows;
void actan(mpz_t res, unsigned long base, mpz_t pows)
{
	int i, neg = 1;
	mpz_tdiv_q_ui(res, pows, base);
	mpz_set(tmp1, res);
	for (i = 3; ; i += 2) {
		mpz_tdiv_q_ui(tmp1, tmp1, base * base);
		mpz_tdiv_q_ui(tmp2, tmp1, i);
		if (mpz_cmp_ui(tmp2, 0) == 0) break;
		if (neg) mpz_sub(res, res, tmp2);
		else	  mpz_add(res, res, tmp2);
		neg = !neg;
	}
}

char * get_digits(int n, size_t* len)
{
	mpz_ui_pow_ui(pows, 10, n + 20);

	actan(t5, 5, pows);
	mpz_mul_ui(t5, t5, 16);

	actan(t239, 239, pows);
	mpz_mul_ui(t239, t239, 4);

	mpz_sub(t5, t5, t239);
	mpz_ui_pow_ui(pows, 10, 20);
	mpz_tdiv_q(t5, t5, pows);

	*len = mpz_sizeinbase(t5, 10);
	return mpz_get_str(0, 0, t5);
}

int main(int c, char **v)
{
	unsigned long accu = 16384, done = 0;
	size_t got;
	char *s;

	mpz_init(tmp1);
	mpz_init(tmp2);
	mpz_init(t5);
	mpz_init(t239);
	mpz_init(pows);

	while (1) {
		s = get_digits(accu, &got);

		/* write out digits up to the last one not preceding a 0 or 9*/
		got -= 2; /* -2: length estimate may be longer than actual */
		while (s[got] == '0' || s[got] == '9') got--;

		printf("%.*s", (int)(got - done), s + done);
		free(s);

		done = got;

		/* double the desired digits; slows down at least cubically */
		accu *= 2;
	}

	return 0;
}
