/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/credential.h"
#include "git2/sys/credential.h"
#include "git2/credential_helpers.h"

static int git_credential_ssh_key_type_new(
	git_credential **cred,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase,
	git_credential_t credtype);

int git_credential_has_username(git_credential *cred)
{
	if (cred->credtype == GIT_CREDENTIAL_DEFAULT)
		return 0;

	return 1;
}

const char *git_credential_get_username(git_credential *cred)
{
	switch (cred->credtype) {
	case GIT_CREDENTIAL_USERNAME:
	{
		git_credential_username *c = (git_credential_username *) cred;
		return c->username;
	}
	case GIT_CREDENTIAL_USERPASS_PLAINTEXT:
	{
		git_credential_userpass_plaintext *c = (git_credential_userpass_plaintext *) cred;
		return c->username;
	}
	case GIT_CREDENTIAL_SSH_KEY:
	case GIT_CREDENTIAL_SSH_MEMORY:
	{
		git_credential_ssh_key *c = (git_credential_ssh_key *) cred;
		return c->username;
	}
	case GIT_CREDENTIAL_SSH_CUSTOM:
	{
		git_credential_ssh_custom *c = (git_credential_ssh_custom *) cred;
		return c->username;
	}
	case GIT_CREDENTIAL_SSH_INTERACTIVE:
	{
		git_credential_ssh_interactive *c = (git_credential_ssh_interactive *) cred;
		return c->username;
	}

	default:
		return NULL;
	}
}

static void plaintext_free(struct git_credential *cred)
{
	git_credential_userpass_plaintext *c = (git_credential_userpass_plaintext *)cred;

	git__free(c->username);

	/* Zero the memory which previously held the password */
	if (c->password) {
		size_t pass_len = strlen(c->password);
		git__memzero(c->password, pass_len);
		git__free(c->password);
	}

	git__free(c);
}

int git_credential_userpass_plaintext_new(
	git_credential **cred,
	const char *username,
	const char *password)
{
	git_credential_userpass_plaintext *c;

	GIT_ASSERT_ARG(cred);
	GIT_ASSERT_ARG(username);
	GIT_ASSERT_ARG(password);

	c = git__malloc(sizeof(git_credential_userpass_plaintext));
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = GIT_CREDENTIAL_USERPASS_PLAINTEXT;
	c->parent.free = plaintext_free;
	c->username = git__strdup(username);

	if (!c->username) {
		git__free(c);
		return -1;
	}

	c->password = git__strdup(password);

	if (!c->password) {
		git__free(c->username);
		git__free(c);
		return -1;
	}

	*cred = &c->parent;
	return 0;
}

static void ssh_key_free(struct git_credential *cred)
{
	git_credential_ssh_key *c =
		(git_credential_ssh_key *)cred;

	git__free(c->username);

	if (c->privatekey) {
		/* Zero the memory which previously held the private key */
		size_t key_len = strlen(c->privatekey);
		git__memzero(c->privatekey, key_len);
		git__free(c->privatekey);
	}

	if (c->passphrase) {
		/* Zero the memory which previously held the passphrase */
		size_t pass_len = strlen(c->passphrase);
		git__memzero(c->passphrase, pass_len);
		git__free(c->passphrase);
	}

	if (c->publickey) {
		/* Zero the memory which previously held the public key */
		size_t key_len = strlen(c->publickey);
		git__memzero(c->publickey, key_len);
		git__free(c->publickey);
	}

	git__free(c);
}

static void ssh_interactive_free(struct git_credential *cred)
{
	git_credential_ssh_interactive *c = (git_credential_ssh_interactive *)cred;

	git__free(c->username);

	git__free(c);
}

static void ssh_custom_free(struct git_credential *cred)
{
	git_credential_ssh_custom *c = (git_credential_ssh_custom *)cred;

	git__free(c->username);

	if (c->publickey) {
		/* Zero the memory which previously held the publickey */
		size_t key_len = strlen(c->publickey);
		git__memzero(c->publickey, key_len);
		git__free(c->publickey);
	}

	git__free(c);
}

static void default_free(struct git_credential *cred)
{
	git_credential_default *c = (git_credential_default *)cred;

	git__free(c);
}

static void username_free(struct git_credential *cred)
{
	git__free(cred);
}

int git_credential_ssh_key_new(
	git_credential **cred,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase)
{
	return git_credential_ssh_key_type_new(
		cred,
		username,
		publickey,
		privatekey,
		passphrase,
		GIT_CREDENTIAL_SSH_KEY);
}

int git_credential_ssh_key_memory_new(
	git_credential **cred,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase)
{
#ifdef GIT_SSH_LIBSSH2_MEMORY_CREDENTIALS
	return git_credential_ssh_key_type_new(
		cred,
		username,
		publickey,
		privatekey,
		passphrase,
		GIT_CREDENTIAL_SSH_MEMORY);
#else
	GIT_UNUSED(cred);
	GIT_UNUSED(username);
	GIT_UNUSED(publickey);
	GIT_UNUSED(privatekey);
	GIT_UNUSED(passphrase);

	git_error_set(GIT_ERROR_INVALID,
		"this version of libgit2 was not built with ssh memory credentials.");
	return -1;
#endif
}

static int git_credential_ssh_key_type_new(
	git_credential **cred,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase,
	git_credential_t credtype)
{
	git_credential_ssh_key *c;

	GIT_ASSERT_ARG(username);
	GIT_ASSERT_ARG(cred);
	GIT_ASSERT_ARG(privatekey);

	c = git__calloc(1, sizeof(git_credential_ssh_key));
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = credtype;
	c->parent.free = ssh_key_free;

	c->username = git__strdup(username);
	GIT_ERROR_CHECK_ALLOC(c->username);

	c->privatekey = git__strdup(privatekey);
	GIT_ERROR_CHECK_ALLOC(c->privatekey);

	if (publickey) {
		c->publickey = git__strdup(publickey);
		GIT_ERROR_CHECK_ALLOC(c->publickey);
	}

	if (passphrase) {
		c->passphrase = git__strdup(passphrase);
		GIT_ERROR_CHECK_ALLOC(c->passphrase);
	}

	*cred = &c->parent;
	return 0;
}

int git_credential_ssh_interactive_new(
	git_credential **out,
	const char *username,
	git_credential_ssh_interactive_cb prompt_callback,
	void *payload)
{
	git_credential_ssh_interactive *c;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(username);
	GIT_ASSERT_ARG(prompt_callback);

	c = git__calloc(1, sizeof(git_credential_ssh_interactive));
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = GIT_CREDENTIAL_SSH_INTERACTIVE;
	c->parent.free = ssh_interactive_free;

	c->username = git__strdup(username);
	GIT_ERROR_CHECK_ALLOC(c->username);

	c->prompt_callback = prompt_callback;
	c->payload = payload;

	*out = &c->parent;
	return 0;
}

int git_credential_ssh_key_from_agent(git_credential **cred, const char *username) {
	git_credential_ssh_key *c;

	GIT_ASSERT_ARG(username);
	GIT_ASSERT_ARG(cred);

	c = git__calloc(1, sizeof(git_credential_ssh_key));
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = GIT_CREDENTIAL_SSH_KEY;
	c->parent.free = ssh_key_free;

	c->username = git__strdup(username);
	GIT_ERROR_CHECK_ALLOC(c->username);

	c->privatekey = NULL;

	*cred = &c->parent;
	return 0;
}

int git_credential_ssh_custom_new(
	git_credential **cred,
	const char *username,
	const char *publickey,
	size_t publickey_len,
	git_credential_sign_cb sign_callback,
	void *payload)
{
	git_credential_ssh_custom *c;

	GIT_ASSERT_ARG(username);
	GIT_ASSERT_ARG(cred);

	c = git__calloc(1, sizeof(git_credential_ssh_custom));
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = GIT_CREDENTIAL_SSH_CUSTOM;
	c->parent.free = ssh_custom_free;

	c->username = git__strdup(username);
	GIT_ERROR_CHECK_ALLOC(c->username);

	if (publickey_len > 0) {
		c->publickey = git__malloc(publickey_len);
		GIT_ERROR_CHECK_ALLOC(c->publickey);

		memcpy(c->publickey, publickey, publickey_len);
	}

	c->publickey_len = publickey_len;
	c->sign_callback = sign_callback;
	c->payload = payload;

	*cred = &c->parent;
	return 0;
}

int git_credential_default_new(git_credential **cred)
{
	git_credential_default *c;

	GIT_ASSERT_ARG(cred);

	c = git__calloc(1, sizeof(git_credential_default));
	GIT_ERROR_CHECK_ALLOC(c);

	c->credtype = GIT_CREDENTIAL_DEFAULT;
	c->free = default_free;

	*cred = c;
	return 0;
}

int git_credential_username_new(git_credential **cred, const char *username)
{
	git_credential_username *c;
	size_t len, allocsize;

	GIT_ASSERT_ARG(cred);

	len = strlen(username);

	GIT_ERROR_CHECK_ALLOC_ADD(&allocsize, sizeof(git_credential_username), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&allocsize, allocsize, 1);
	c = git__malloc(allocsize);
	GIT_ERROR_CHECK_ALLOC(c);

	c->parent.credtype = GIT_CREDENTIAL_USERNAME;
	c->parent.free = username_free;
	memcpy(c->username, username, len + 1);

	*cred = (git_credential *) c;
	return 0;
}

void git_credential_free(git_credential *cred)
{
	if (!cred)
		return;

	cred->free(cred);
}

/* Deprecated credential functions */

#ifndef GIT_DEPRECATE_HARD
int git_cred_has_username(git_credential *cred)
{
	return git_credential_has_username(cred);
}

const char *git_cred_get_username(git_credential *cred)
{
	return git_credential_get_username(cred);
}

int git_cred_userpass_plaintext_new(
	git_credential **out,
	const char *username,
	const char *password)
{
	return git_credential_userpass_plaintext_new(out,username, password);
}

int git_cred_default_new(git_credential **out)
{
	return git_credential_default_new(out);
}

int git_cred_username_new(git_credential **out, const char *username)
{
	return git_credential_username_new(out, username);
}

int git_cred_ssh_key_new(
	git_credential **out,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase)
{
	return git_credential_ssh_key_new(out, username,
		publickey, privatekey, passphrase);
}

int git_cred_ssh_key_memory_new(
	git_credential **out,
	const char *username,
	const char *publickey,
	const char *privatekey,
	const char *passphrase)
{
	return git_credential_ssh_key_memory_new(out, username,
		publickey, privatekey, passphrase);
}

int git_cred_ssh_interactive_new(
	git_credential **out,
	const char *username,
	git_credential_ssh_interactive_cb prompt_callback,
	void *payload)
{
	return git_credential_ssh_interactive_new(out, username,
		prompt_callback, payload);
}

int git_cred_ssh_key_from_agent(
	git_credential **out,
	const char *username)
{
	return git_credential_ssh_key_from_agent(out, username);
}

int git_cred_ssh_custom_new(
	git_credential **out,
	const char *username,
	const char *publickey,
	size_t publickey_len,
	git_credential_sign_cb sign_callback,
	void *payload)
{
	return git_credential_ssh_custom_new(out, username,
		publickey, publickey_len, sign_callback, payload);
}

void git_cred_free(git_credential *cred)
{
	git_credential_free(cred);
}
#endif
