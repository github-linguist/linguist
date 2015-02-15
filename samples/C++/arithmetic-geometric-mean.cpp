/*Arithmetic Geometric Mean of 1 and 1/sqrt(2)

  Nigel_Galloway
  February 7th., 2012.
*/

#include "gmp.h"

void agm (const mpf_t in1, const mpf_t in2, mpf_t out1, mpf_t out2) {
	mpf_add (out1, in1, in2);
	mpf_div_ui (out1, out1, 2);
	mpf_mul (out2, in1, in2);
	mpf_sqrt (out2, out2);
}

int main (void) {
	mpf_set_default_prec (65568);
	mpf_t x0, y0, resA, resB;

	mpf_init_set_ui (y0, 1);
	mpf_init_set_d (x0, 0.5);
	mpf_sqrt (x0, x0);
	mpf_init (resA);
	mpf_init (resB);

	for(int i=0; i<7; i++){
		agm(x0, y0, resA, resB);
		agm(resA, resB, x0, y0);
	}
	gmp_printf ("%.20000Ff\n", x0);
	gmp_printf ("%.20000Ff\n\n", y0);

	return 0;
}
