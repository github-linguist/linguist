#include "gmp.h"

void agm (const mpf_t in1, const mpf_t in2, mpf_t out1, mpf_t out2) {
	mpf_add (out1, in1, in2);
	mpf_div_ui (out1, out1, 2);
	mpf_mul (out2, in1, in2);
	mpf_sqrt (out2, out2);
}

int main (void) {
	mpf_set_default_prec (300000);
	mpf_t x0, y0, resA, resB, Z, var;

	mpf_init_set_ui (x0, 1);
	mpf_init_set_d (y0, 0.5);
	mpf_sqrt (y0, y0);
	mpf_init (resA);
	mpf_init (resB);
	mpf_init_set_d (Z, 0.25);
	mpf_init (var);

	int n = 1;
	for(int i=0; i<8; i++){
		agm(x0, y0, resA, resB);
		mpf_sub(var, resA, x0);
		mpf_mul(var, var, var);
		mpf_mul_ui(var, var, n);
		mpf_sub(Z, Z, var);
		n += n;
		agm(resA, resB, x0, y0);
		mpf_sub(var, x0, resA);
		mpf_mul(var, var, var);
		mpf_mul_ui(var, var, n);
		mpf_sub(Z, Z, var);
		n += n;
	}
	mpf_mul(x0, x0, x0);
	mpf_div(x0, x0, Z);
	gmp_printf ("%.100000Ff\n", x0);
	return 0;
}
