/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

VALUE rugged_signature_new(const git_signature *sig, const char *encoding_name)
{
	VALUE rb_sig, rb_time;
	rb_encoding *encoding = rb_utf8_encoding();

	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	rb_sig = rb_hash_new();

	/* Allocate the time with a the given timezone */
	rb_time = rb_funcall(
		rb_time_new(sig->when.time, 0),
		rb_intern("getlocal"), 1,
		INT2FIX(sig->when.offset * 60)
	);

	rb_hash_aset(rb_sig, CSTR2SYM("name"),
		rb_enc_str_new(sig->name, strlen(sig->name), encoding));

	rb_hash_aset(rb_sig, CSTR2SYM("email"),
		rb_enc_str_new(sig->email, strlen(sig->email), encoding));

	rb_hash_aset(rb_sig, CSTR2SYM("time"), rb_time);

	return rb_sig;
}

VALUE rugged_signature_from_buffer(const char *buffer, const char *encoding_name)
{
	git_signature *sig;
	VALUE rb_ret;

	rugged_exception_check(git_signature_from_buffer(&sig, buffer));

	rb_ret = rugged_signature_new(sig, encoding_name);
	git_signature_free(sig);

	return rb_ret;
}

git_signature *rugged_signature_get(VALUE rb_sig, git_repository *repo)
{
	int error;
	VALUE rb_time, rb_unix_t, rb_offset, rb_name, rb_email, rb_time_offset;
	git_signature *sig;

	if (NIL_P(rb_sig)) {
		rugged_exception_check(
			git_signature_default(&sig, repo)
		);
		return sig;
	}

	Check_Type(rb_sig, T_HASH);

	rb_name = rb_hash_fetch(rb_sig, CSTR2SYM("name"));
	rb_email = rb_hash_fetch(rb_sig, CSTR2SYM("email"));
	rb_time = rb_hash_aref(rb_sig, CSTR2SYM("time"));
	rb_time_offset = rb_hash_aref(rb_sig, CSTR2SYM("time_offset"));

	Check_Type(rb_name, T_STRING);
	Check_Type(rb_email, T_STRING);


	if (NIL_P(rb_time)) {
		error = git_signature_now(&sig,
				StringValueCStr(rb_name),
				StringValueCStr(rb_email));
	} else {
		if (!rb_obj_is_kind_of(rb_time, rb_cTime))
			rb_raise(rb_eTypeError, "expected Time object");

		rb_unix_t = rb_funcall(rb_time, rb_intern("tv_sec"), 0);

		if (NIL_P(rb_time_offset)) {
			rb_offset = rb_funcall(rb_time, rb_intern("utc_offset"), 0);
		} else {
			Check_Type(rb_time_offset, T_FIXNUM);
			rb_offset = rb_time_offset;
		}

		error = git_signature_new(&sig,
				StringValueCStr(rb_name),
				StringValueCStr(rb_email),
				NUM2LONG(rb_unix_t),
				FIX2INT(rb_offset) / 60);
	}

	rugged_exception_check(error);

	return sig;
}
