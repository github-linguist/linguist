#include <gmp.h>

int main()
{
	mpz_t a, b, m, r;

	mpz_init_set_str(a,	"2988348162058574136915891421498819466320"
				"163312926952423791023078876139", 0);
	mpz_init_set_str(b,	"2351399303373464486466122544523690094744"
				"975233415544072992656881240319", 0);
	mpz_init(m);
	mpz_ui_pow_ui(m, 10, 40);

	mpz_init(r);
	mpz_powm(r, a, b, m);

	gmp_printf("%Zd\n", r); /* ...16808958343740453059 */

	mpz_clear(a);
	mpz_clear(b);
	mpz_clear(m);
	mpz_clear(r);

	return 0;
}
