/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
VALUE rb_mRuggedCred;
VALUE rb_cRuggedCredUserPassword;
VALUE rb_cRuggedCredSshKey;
VALUE rb_cRuggedCredSshKeyFromAgent;
VALUE rb_cRuggedCredDefault;

static void rugged_cred_extract_userpass(git_cred **cred, VALUE rb_credential)
{
	VALUE rb_username = rb_iv_get(rb_credential, "@username");
	VALUE rb_password = rb_iv_get(rb_credential, "@password");

	Check_Type(rb_username, T_STRING);
	Check_Type(rb_password, T_STRING);

	rugged_exception_check(
		git_cred_userpass_plaintext_new(cred,
			StringValueCStr(rb_username),
			StringValueCStr(rb_password)
		)
	);
}

static void rugged_cred_extract_ssh_key(git_cred **cred, VALUE rb_credential)
{
	VALUE rb_username   = rb_iv_get(rb_credential, "@username");
	VALUE rb_publickey  = rb_iv_get(rb_credential, "@publickey");
	VALUE rb_privatekey = rb_iv_get(rb_credential, "@privatekey");
	VALUE rb_passphrase = rb_iv_get(rb_credential, "@passphrase");

	Check_Type(rb_username, T_STRING);
	Check_Type(rb_privatekey, T_STRING);

	if (!NIL_P(rb_publickey))
		Check_Type(rb_publickey, T_STRING);
	if (!NIL_P(rb_passphrase))
		Check_Type(rb_passphrase, T_STRING);

	rugged_exception_check(
		git_cred_ssh_key_new(cred,
			StringValueCStr(rb_username),
			NIL_P(rb_publickey) ? NULL : StringValueCStr(rb_publickey),
			StringValueCStr(rb_privatekey),
			NIL_P(rb_passphrase) ? NULL : StringValueCStr(rb_passphrase)
		)
	);
}

static void rugged_credential_extract_ssh_key_from_agent(git_cred **cred, VALUE rb_credential)
{
	VALUE rb_username = rb_iv_get(rb_credential, "@username");

	Check_Type(rb_username, T_STRING);

	rugged_exception_check(
		git_cred_ssh_key_from_agent(cred, StringValueCStr(rb_username))
	);
}

static void rugged_cred_extract_default(git_cred **cred, VALUE rb_credential)
{
	rugged_exception_check(git_cred_default_new(cred));
}

static void rugged_cred_extract_username(git_cred **cred, VALUE rb_credential)
{
	VALUE rb_username = rb_iv_get(rb_credential, "@username");
	Check_Type(rb_username, T_STRING);

	rugged_exception_check(git_cred_username_new(cred, StringValueCStr(rb_username)));
}

void rugged_cred_extract(git_cred **cred, int allowed_types, VALUE rb_credential)
{
	if (rb_obj_is_kind_of(rb_credential, rb_cRuggedCredUserPassword)) {
		if (allowed_types & GIT_CREDTYPE_USERNAME) {
			rugged_cred_extract_username(cred, rb_credential);
			return;
		}

		if (!(allowed_types & GIT_CREDTYPE_USERPASS_PLAINTEXT))
			rb_raise(rb_eArgError, "Invalid credential type");

		rugged_cred_extract_userpass(cred, rb_credential);
	} else if (rb_obj_is_kind_of(rb_credential, rb_cRuggedCredSshKey)) {
		if (allowed_types & GIT_CREDTYPE_USERNAME) {
			rugged_cred_extract_username(cred, rb_credential);
			return;
		}

		if (!(allowed_types & GIT_CREDTYPE_SSH_KEY))
			rb_raise(rb_eArgError, "Invalid credential type");

		rugged_cred_extract_ssh_key(cred, rb_credential);
	} else if (rb_obj_is_kind_of(rb_credential, rb_cRuggedCredSshKeyFromAgent)) {
		if (allowed_types & GIT_CREDTYPE_USERNAME) {
			rugged_cred_extract_username(cred, rb_credential);
			return;
		}

		if (!(allowed_types & GIT_CREDTYPE_SSH_KEY))
			rb_raise(rb_eArgError, "Invalid credential type");

		rugged_credential_extract_ssh_key_from_agent(cred, rb_credential);
	} else if (rb_obj_is_kind_of(rb_credential, rb_cRuggedCredDefault)) {
		if (!(allowed_types & GIT_CREDTYPE_DEFAULT))
			rb_raise(rb_eArgError, "Invalid credential type");

		rugged_cred_extract_default(cred, rb_credential);
	}
}


void Init_rugged_cred(void)
{
	rb_mRuggedCred                = rb_define_module_under(rb_mRugged, "Credentials");

	rb_cRuggedCredUserPassword    = rb_define_class_under(rb_mRuggedCred, "UserPassword", rb_cObject);
	rb_cRuggedCredSshKey          = rb_define_class_under(rb_mRuggedCred, "SshKey", rb_cObject);
	rb_cRuggedCredSshKeyFromAgent = rb_define_class_under(rb_mRuggedCred, "SshKeyFromAgent", rb_cObject);
	rb_cRuggedCredDefault         = rb_define_class_under(rb_mRuggedCred, "Default", rb_cObject);
}
