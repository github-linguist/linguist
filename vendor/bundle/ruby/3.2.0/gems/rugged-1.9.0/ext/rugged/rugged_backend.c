/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;

VALUE rb_cRuggedBackend;

void Init_rugged_backend(void)
{
	rb_cRuggedBackend = rb_define_class_under(rb_mRugged, "Backend", rb_cObject);
}
