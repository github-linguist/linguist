/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#ifndef __H_RUGGED_BINDINGS__
#define __H_RUGGED_BINDINGS__

// tell rbx not to use it's caching compat layer
// by doing this we're making a promize to RBX that
// we'll never modify the pointers we get back from RSTRING_PTR
#define RSTRING_NOT_MODIFIED

#include <ruby.h>
#ifndef HAVE_RUBY_ENCODING_H
#error "Rugged requires Ruby 1.9+ to build"
#else
#include <ruby/encoding.h>
#endif

#include <ruby/util.h>

#include <assert.h>
#include <git2.h>
#include <git2/odb_backend.h>
#include <git2/sys/path.h>

#define rb_str_new_utf8(str) rb_enc_str_new(str, strlen(str), rb_utf8_encoding())
#define CSTR2SYM(s) (ID2SYM(rb_intern((s))))

/*
 * Initialization functions
 */
void Init_rugged_object(void);
void Init_rugged_branch(void);
void Init_rugged_branch_collection(void);
void Init_rugged_commit(void);
void Init_rugged_tree(void);
void Init_rugged_tag(void);
void Init_rugged_tag_collection(void);
void Init_rugged_blob(void);
void Init_rugged_index(void);
void Init_rugged_repo(void);
void Init_rugged_revwalk(void);
void Init_rugged_reference(void);
void Init_rugged_reference_collection(void);
void Init_rugged_config(void);
void Init_rugged_remote(void);
void Init_rugged_remote_collection(void);
void Init_rugged_notes(void);
void Init_rugged_settings(void);
void Init_rugged_submodule(void);
void Init_rugged_submodule_collection(void);
void Init_rugged_diff(void);
void Init_rugged_patch(void);
void Init_rugged_diff_delta(void);
void Init_rugged_diff_hunk(void);
void Init_rugged_diff_line(void);
void Init_rugged_blame(void);
void Init_rugged_cred(void);
void Init_rugged_backend(void);
void Init_rugged_rebase(void);

VALUE rb_git_object_init(git_otype type, int argc, VALUE *argv, VALUE self);

VALUE rugged_raw_read(git_repository *repo, const git_oid *oid);

VALUE rugged_signature_new(const git_signature *sig, const char *encoding_name);

VALUE rugged_repo_new(VALUE klass, git_repository *repo);
VALUE rugged_index_new(VALUE klass, VALUE owner, git_index *index);
VALUE rugged_config_new(VALUE klass, VALUE owner, git_config *cfg);
VALUE rugged_object_new(VALUE owner, git_object *object);
VALUE rugged_object_rev_parse(VALUE rb_repo, VALUE rb_spec, int as_obj);
VALUE rugged_ref_new(VALUE klass, VALUE owner, git_reference *ref);
VALUE rugged_diff_new(VALUE klass, VALUE owner, git_diff *diff);
VALUE rugged_patch_new(VALUE owner, git_patch *patch);
VALUE rugged_diff_delta_new(VALUE owner, const git_diff_delta *delta);
VALUE rugged_diff_hunk_new(VALUE owner, size_t hunk_idx, const git_diff_hunk *hunk, size_t lines_in_hunk);
VALUE rugged_diff_line_new(const git_diff_line *line);
VALUE rugged_remote_new(VALUE owner, git_remote *remote);
VALUE rb_git_delta_file_fromC(const git_diff_file *file);
VALUE rb_merge_file_result_fromC(const git_merge_file_result *results);

void rugged_parse_diff_options(git_diff_options *opts, VALUE rb_options);
void rugged_parse_merge_options(git_merge_options *opts, VALUE rb_options);
void rugged_parse_checkout_options(git_checkout_options *opts, VALUE rb_options);
void rugged_parse_merge_file_options(git_merge_file_options *opts, VALUE rb_options);

void rugged_cred_extract(git_cred **cred, int allowed_types, VALUE rb_credential);

VALUE rugged_otype_new(git_otype t);
git_otype rugged_otype_get(VALUE rb_type);

git_signature *rugged_signature_get(VALUE rb_person, git_repository *repo);
git_object *rugged_object_get(git_repository *repo, VALUE object_value, git_otype type);
int rugged_oid_get(git_oid *oid, git_repository *repo, VALUE p);
const char * rugged_refname_from_string_or_ref(VALUE rb_name_or_ref);

VALUE rugged_signature_from_buffer(const char *buffer, const char *encoding_name);

void rugged_rb_ary_to_strarray(VALUE rb_array, git_strarray *str_array);
VALUE rugged_strarray_to_rb_ary(git_strarray *str_array);

#define CALLABLE_OR_RAISE(ret, name) \
	do { \
		if (!rb_respond_to(ret, rb_intern("call"))) \
			rb_raise(rb_eArgError, "Expected a Proc or an object that responds to #call (:" name " )."); \
	} while (0);

static inline void rugged_set_owner(VALUE object, VALUE owner)
{
	rb_iv_set(object, "@owner", owner);
}

static inline VALUE rugged_owner(VALUE object)
{
	return rb_iv_get(object, "@owner");
}

extern void rugged_exception_raise(void);

static inline void rugged_exception_check(int errorcode)
{
	if (errorcode < 0)
		rugged_exception_raise();
}

static inline int rugged_parse_bool(VALUE boolean)
{
	if (TYPE(boolean) != T_TRUE && TYPE(boolean) != T_FALSE)
		rb_raise(rb_eTypeError, "Expected boolean value");

	return boolean ? 1 : 0;
}

extern VALUE rb_cRuggedRepo;

VALUE rugged__block_yield_splat(VALUE args);

struct rugged_apply_cb_payload
{
	VALUE delta_cb;
	VALUE hunk_cb;
	int exception;
};

struct rugged_cb_payload
{
    VALUE rb_data;
    int exception;
};

struct rugged_remote_cb_payload
{
	VALUE progress;
	VALUE completion;
	VALUE transfer_progress;
	VALUE update_tips;
	VALUE credentials;
	VALUE certificate_check;
	VALUE result;
	int exception;
};

void rugged_remote_init_custom_headers(VALUE rb_options, git_strarray *custom_headers);

void rugged_remote_init_proxy_options(VALUE rb_options, git_proxy_options *proxy_options);

void rugged_remote_init_callbacks_and_payload_from_options(
	VALUE rb_options,
	git_remote_callbacks *callbacks,
	struct rugged_remote_cb_payload *payload);

static inline void rugged_check_repo(VALUE rb_repo)
{
	if (!rb_obj_is_kind_of(rb_repo, rb_cRuggedRepo))
		rb_raise(rb_eTypeError, "Expecting a Rugged Repository");
}

static inline VALUE rugged_create_oid(const git_oid *oid)
{
	char out[40];
	git_oid_fmt(out, oid);
	return rb_usascii_str_new(out, 40);
}


typedef struct _rugged_backend {
  int (* odb_backend)(git_odb_backend **backend_out, struct _rugged_backend *backend, const char* path);
  int (* refdb_backend)(git_refdb_backend **backend_out, struct _rugged_backend *backend, const char* path);
} rugged_backend;

extern void rugged_set_allocator(void);

#endif
