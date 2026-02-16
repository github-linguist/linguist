#include "common.h"

VALUE rb_mCharlockHolmes;

void Init_charlock_holmes(void) {
	rb_mCharlockHolmes = rb_define_module("CharlockHolmes");

	_init_charlock_encoding_detector();
	_init_charlock_converter();
	_init_charlock_transliterator();
}
