/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include <git2/sys/repository.h>
#include <git2/sys/odb_backend.h>
#include <git2/sys/refdb_backend.h>
#include <git2/refs.h>
#include <git2/apply.h>

extern VALUE rb_mRugged;
extern VALUE rb_eRuggedError;
extern VALUE rb_cRuggedIndex;
extern VALUE rb_cRuggedConfig;
extern VALUE rb_cRuggedBackend;
extern VALUE rb_cRuggedRemote;
extern VALUE rb_cRuggedCommit;
extern VALUE rb_cRuggedDiff;
extern VALUE rb_cRuggedTag;
extern VALUE rb_cRuggedTree;
extern VALUE rb_cRuggedReference;
extern VALUE rb_cRuggedBackend;

extern VALUE rb_cRuggedCredPlaintext;
extern VALUE rb_cRuggedCredSshKey;
extern VALUE rb_cRuggedCredDefault;

VALUE rb_cRuggedRepo;
VALUE rb_cRuggedOdbObject;

static ID id_call;

extern const rb_data_type_t rugged_object_type;
static const rb_data_type_t rugged_odb_object_type;

/*
 *  call-seq:
 *    odb_obj.oid -> hex_oid
 *
 *  Return the Object ID (a 40 character SHA1 hash) for this raw
 *  object.
 *
 *    odb_obj.oid #=> "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f"
 */
static VALUE rb_git_odbobj_oid(VALUE self)
{
	git_odb_object *obj;
	TypedData_Get_Struct(self, git_odb_object, &rugged_odb_object_type, obj);
	return rugged_create_oid(git_odb_object_id(obj));
}

/*
 *  call-seq:
 *    odb_obj.data -> buffer
 *
 *  Return an ASCII buffer with the raw bytes that form the Git object.
 *
 *    odb_obj.data #=> "tree 87ebee8367f9cc5ac04858b3bd5610ca74f04df9\n"
 *                 #=> "parent 68d041ee999cb07c6496fbdd4f384095de6ca9e1\n"
 *                 #=> "author Vicent Martí <tanoku@gmail.com> 1326863045 -0800\n"
 *                 #=> ...
 */
static VALUE rb_git_odbobj_data(VALUE self)
{
	git_odb_object *obj;
	TypedData_Get_Struct(self, git_odb_object, &rugged_odb_object_type, obj);
	return rb_str_new(git_odb_object_data(obj), git_odb_object_size(obj));
}

/*
 *  call-seq:
 *    odb_obj.size -> size
 *
 *  Return the size in bytes of the Git object after decompression. This is
 *  also the size of the +obj.data+ buffer.
 *
 *    odb_obj.size #=> 231
 */
static VALUE rb_git_odbobj_size(VALUE self)
{
	git_odb_object *obj;
	TypedData_Get_Struct(self, git_odb_object, &rugged_odb_object_type, obj);
	return INT2FIX(git_odb_object_size(obj));
}

/*
 *  call-seq:
 *    odb_obj.type -> Symbol
 *
 *  Return a Ruby symbol representing the basic Git type of this object.
 *  Possible values are +:tree+, +:blob+, +:commit+ and +:tag+.
 *
 *    odb_obj.type #=> :tag
 */
static VALUE rb_git_odbobj_type(VALUE self)
{
	git_odb_object *obj;
	TypedData_Get_Struct(self, git_odb_object, &rugged_odb_object_type, obj);
	return rugged_otype_new(git_odb_object_type(obj));
}

void rb_git__odbobj_free(void *obj)
{
	git_odb_object_free((git_odb_object *)obj);
}

static size_t rb_git__odbobj_size(const void *obj)
{
	return git_odb_object_size((git_odb_object *)obj);
}

static const rb_data_type_t rugged_odb_object_type = {
	.wrap_struct_name = "Rugged::OdbObject",
	.function = {
		.dmark = NULL,
		.dfree = rb_git__odbobj_free,
		.dsize = rb_git__odbobj_size,
	},
	.data = NULL,
	.flags = RUBY_TYPED_FREE_IMMEDIATELY,
};



VALUE rugged_raw_read(git_repository *repo, const git_oid *oid)
{
	git_odb *odb;
	git_odb_object *obj;

	int error;

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	error = git_odb_read(&obj, odb, oid);
	git_odb_free(odb);
	rugged_exception_check(error);

	return TypedData_Wrap_Struct(rb_cRuggedOdbObject, &rugged_odb_object_type, obj);
}

void rb_git_repo__free(git_repository *repo)
{
	git_repository_free(repo);
}

VALUE rugged_repo_new(VALUE klass, git_repository *repo)
{
	VALUE rb_repo = Data_Wrap_Struct(klass, NULL, &rb_git_repo__free, repo);

#ifdef HAVE_RUBY_ENCODING_H
	/* TODO: set this properly */
	rb_iv_set(rb_repo, "@encoding",
		rb_enc_from_encoding(rb_filesystem_encoding()));
#endif

	rb_iv_set(rb_repo, "@config", Qnil);
	rb_iv_set(rb_repo, "@index", Qnil);

	return rb_repo;
}

static void load_alternates(git_repository *repo, VALUE rb_alternates)
{
	git_odb *odb = NULL;
	int i, error;

	if (NIL_P(rb_alternates))
		return;

	Check_Type(rb_alternates, T_ARRAY);

	if (RARRAY_LEN(rb_alternates) == 0)
		return;

	for (i = 0; i < RARRAY_LEN(rb_alternates); ++i)
		Check_Type(rb_ary_entry(rb_alternates, i), T_STRING);

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	for (i = 0; !error && i < RARRAY_LEN(rb_alternates); ++i) {
		VALUE alt = rb_ary_entry(rb_alternates, i);
		error = git_odb_add_disk_alternate(odb, StringValueCStr(alt));
	}

	git_odb_free(odb);
	rugged_exception_check(error);
}

static void rugged_repo_new_with_backend(git_repository **repo, VALUE rb_path, VALUE rb_backend)
{
	char *path;

	git_odb *odb = NULL;
	git_odb_backend *odb_backend = NULL;
	git_refdb *refdb = NULL;
	git_refdb_backend *refdb_backend = NULL;
	git_reference *head = NULL;
	rugged_backend *backend;

	int error = 0;

	FilePathValue(rb_path);
	path = StringValueCStr(rb_path);

	if (rb_obj_is_kind_of(rb_backend, rb_cRuggedBackend) == Qfalse) {
		rb_raise(rb_eRuggedError, "Backend must be an instance of Rugged::Backend");
	}

	Data_Get_Struct(rb_backend, rugged_backend, backend);

	error = git_odb_new(&odb);
	if (error) goto cleanup;

	error = backend->odb_backend(&odb_backend, backend, path);
	if (error) goto cleanup;

	error = git_odb_add_backend(odb, odb_backend, 1);
	if (error) {
		assert(odb_backend->free);
		odb_backend->free(odb_backend);
		goto cleanup;
	}

	error = git_repository_wrap_odb(repo, odb);
	if (error) goto cleanup;

	error = git_refdb_new(&refdb, *repo);
	if (error) goto cleanup;

	error = backend->refdb_backend(&refdb_backend, backend, path);
	if (error) {
		assert(refdb_backend->free);
		refdb_backend->free(refdb_backend);
		goto cleanup;
	}

	error = git_refdb_set_backend(refdb, refdb_backend);
	if (error) goto cleanup;

	git_repository_set_refdb(*repo, refdb);

	error = git_reference_lookup(&head, *repo, "HEAD");

	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		error = git_reference_symbolic_create(&head, *repo, "HEAD", "refs/heads/master", 0, NULL);
	}

	if (!error) {
		git_reference_free(head);
		return;
	}

cleanup:
	git_repository_free(*repo);
	git_odb_free(odb);
	git_refdb_free(refdb);

	rugged_exception_check(error);
}

/*
 *  call-seq:
 *    Repository.bare(path[, alternates]) -> repository OR
 *    Repository.bare(path[, options]) -> repository
 *
 *  Open a bare Git repository at +path+ and return a +Repository+
 *  object representing it.
 *
 *  This is faster than Rugged::Repository.new, as it won't attempt to perform
 *  any +.git+ directory discovery, won't try to load the config options to
 *  determine whether the repository is bare and won't try to load the workdir.
 *
 *  Optionally, you can pass a list of alternate object folders or an options Hash.
 *
 *    Rugged::Repository.bare(path, ['./other/repo/.git/objects'])
 *    Rugged::Repository.bare(path, opts)
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :backend ::
 *    A Rugged::Backend instance
 *  :alternates ::
 *    A list of alternate object folders.
 *    Rugged::Repository.bare(path, :alternates => ['./other/repo/.git/objects'])
 */
static VALUE rb_git_repo_open_bare(int argc, VALUE *argv, VALUE klass)
{
	git_repository *repo = NULL;
	int error = 0;
	VALUE rb_path, rb_options, rb_alternates = 0;

	rb_scan_args(argc, argv, "11", &rb_path, &rb_options);

	if (!NIL_P(rb_options) && TYPE(rb_options) == T_ARRAY)
		rb_alternates = rb_options;

	if (!NIL_P(rb_options) && TYPE(rb_options) == T_HASH) {
		/* Check for `:backend` */
		VALUE rb_backend = rb_hash_aref(rb_options, CSTR2SYM("backend"));

		if (!NIL_P(rb_backend)) {
			rugged_repo_new_with_backend(&repo, rb_path, rb_backend);
		}

		/* Check for `:alternates` */
		rb_alternates = rb_hash_aref(rb_options, CSTR2SYM("alternates"));
	}

	if (!repo) {
		FilePathValue(rb_path);

		error = git_repository_open_bare(&repo, StringValueCStr(rb_path));
		rugged_exception_check(error);
	}

	if (rb_alternates) {
		load_alternates(repo, rb_alternates);
	}

	return rugged_repo_new(klass, repo);
}

/*
 *  call-seq:
 *    Repository.new(path, options = {}) -> repository
 *
 *  Open a Git repository in the given +path+ and return a +Repository+ object
 *  representing it. An exception will be thrown if +path+ doesn't point to a
 *  valid repository. If you need to create a repository from scratch, use
 *  Rugged::Repository.init_at instead.
 *
 *  The +path+ must point to either the actual folder (+.git+) of a Git repository,
 *  or to the directorly that contains the +.git+ folder.
 *
 *  See also Rugged::Repository.discover and Rugged::Repository.bare.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :alternates ::
 *    A list of alternate object folders.
 *
 *  Examples:
 *
 *    Rugged::Repository.new('test/.git') #=> #<Rugged::Repository:0x108849488>
 *    Rugged::Repository.new(path, :alternates => ['./other/repo/.git/objects'])
 */
static VALUE rb_git_repo_new(int argc, VALUE *argv, VALUE klass)
{
	git_repository *repo;
	int error = 0;
	VALUE rb_path, rb_options;

	rb_scan_args(argc, argv, "10:", &rb_path, &rb_options);
	FilePathValue(rb_path);

	error = git_repository_open(&repo, StringValueCStr(rb_path));
	rugged_exception_check(error);

	if (!NIL_P(rb_options)) {
		/* Check for `:alternates` */
		load_alternates(repo, rb_hash_aref(rb_options, CSTR2SYM("alternates")));
	}

	return rugged_repo_new(klass, repo);
}

/*
 *  call-seq:
 *    Repository.init_at(path, is_bare = false, opts = {}) -> repository
 *
 *  Initialize a Git repository in +path+. This implies creating all the
 *  necessary files on the FS, or re-initializing an already existing
 *  repository if the files have already been created.
 *
 *  The +is_bare+ (optional, defaults to false) attribute specifies whether
 *  the Repository should be created on disk as bare or not.
 *  Bare repositories have no working directory and are created in the root
 *  of +path+. Non-bare repositories are created in a +.git+ folder and
 *  use +path+ as working directory.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :backend ::
 *    A Rugged::Backend instance
 *
 *
 *    Rugged::Repository.init_at('repository', :bare) #=> #<Rugged::Repository:0x108849488>
 */
static VALUE rb_git_repo_init_at(int argc, VALUE *argv, VALUE klass)
{
	git_repository *repo = NULL;
	VALUE rb_path, rb_is_bare, rb_options;
	int error;

	rb_scan_args(argc, argv, "11:", &rb_path, &rb_is_bare, &rb_options);
	FilePathValue(rb_path);

	if (!NIL_P(rb_options)) {
		/* Check for `:backend` */
		VALUE rb_backend = rb_hash_aref(rb_options, CSTR2SYM("backend"));

		if (rb_backend && !NIL_P(rb_backend)) {
			rugged_repo_new_with_backend(&repo, rb_path, rb_backend);
		}
	}

	if(!repo) {
		error =	git_repository_init(&repo, StringValueCStr(rb_path), RTEST(rb_is_bare));
		rugged_exception_check(error);
	}

	return rugged_repo_new(klass, repo);
}

static int apply_cb_result(int exception, VALUE result)
{
	if (exception || result == Qnil) {
		return GIT_EAPPLYFAIL;
	} else {
		if (RTEST(result)) {
			return 0;
		} else {
			return 1;
		}
	}
}

static int apply_delta_cb(const git_diff_delta *delta, void *data)
{
	struct rugged_apply_cb_payload *payload = data;
	VALUE args = rb_ary_new2(2);
	VALUE result;

	if (NIL_P(payload->delta_cb))
		return 0;

	VALUE rb_delta = rugged_diff_delta_new(Qnil, delta);

	rb_ary_push(args, payload->delta_cb);
	rb_ary_push(args, rb_delta);

	result = rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return apply_cb_result(payload->exception, result);
}

static int apply_hunk_cb(const git_diff_hunk *hunk, void *data)
{
	struct rugged_apply_cb_payload *payload = data;
	VALUE args = rb_ary_new2(2);
	VALUE result;

	if (NIL_P(payload->hunk_cb))
		return 0;

	VALUE rb_hunk = rugged_diff_hunk_new(Qnil, 0, hunk, 0);

	rb_ary_push(args, payload->hunk_cb);
	rb_ary_push(args, rb_hunk);

	result = rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return apply_cb_result(payload->exception, result);
}

static void rugged_parse_apply_options(git_apply_options *opts, git_apply_location_t *location, VALUE rb_options, struct rugged_apply_cb_payload *payload)
{
	if (!NIL_P(rb_options)) {
		VALUE rb_value;
		Check_Type(rb_options, T_HASH);

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("location"));
		if (!NIL_P(rb_value)) {
			ID id_location;

			Check_Type(rb_value, T_SYMBOL);
			id_location = SYM2ID(rb_value);

			if (id_location == rb_intern("both")) {
				*location = GIT_APPLY_LOCATION_BOTH;
			} else if (id_location == rb_intern("index")) {
				*location = GIT_APPLY_LOCATION_INDEX;
			} else if (id_location == rb_intern("workdir")) {
				*location = GIT_APPLY_LOCATION_WORKDIR;
			} else {
				rb_raise(rb_eTypeError,
					"Invalid location. Expected `:both`, `:index`, or `:workdir`");
			}
		}

		opts->payload = payload;

		payload->delta_cb = rb_hash_aref(rb_options, CSTR2SYM("delta_callback"));
		if (!NIL_P(payload->delta_cb)) {
			CALLABLE_OR_RAISE(payload->delta_cb, "delta_callback");
			opts->delta_cb = apply_delta_cb;
		}

		payload->hunk_cb = rb_hash_aref(rb_options, CSTR2SYM("hunk_callback"));
		if (!NIL_P(payload->hunk_cb)) {
			CALLABLE_OR_RAISE(payload->hunk_cb, "hunk_callback");
			opts->hunk_cb = apply_hunk_cb;
		}
	}
}

static void parse_clone_options(git_clone_options *ret, VALUE rb_options, struct rugged_remote_cb_payload *remote_payload)
{
	VALUE val;

	if (NIL_P(rb_options))
		return;

	val = rb_hash_aref(rb_options, CSTR2SYM("bare"));
	if (RTEST(val))
		ret->bare = 1;

	val = rb_hash_aref(rb_options, CSTR2SYM("checkout_branch"));
	if (!NIL_P(val)) {
		Check_Type(val, T_STRING);
		ret->checkout_branch = StringValueCStr(val);
	}

	rugged_remote_init_callbacks_and_payload_from_options(rb_options, &ret->fetch_opts.callbacks, remote_payload);
	rugged_remote_init_custom_headers(rb_options, &ret->fetch_opts.custom_headers);
	rugged_remote_init_proxy_options(rb_options, &ret->fetch_opts.proxy_opts);
}

/*
 *  call-seq:
 *    Repository.clone_at(url, local_path[, options]) -> repository
 *
 *  Clone a repository from +url+ to +local_path+.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :bare ::
 *    If +true+, the clone will be created as a bare repository.
 *    Defaults to +false+.
 *
 *  :checkout_branch ::
 *    The name of a branch to checkout. Defaults to the remote's +HEAD+.
 *
 *  :remote ::
 *    The name to give to the "origin" remote. Defaults to <tt>"origin"</tt>.
 *
 *  :ignore_cert_errors ::
 *    If set to +true+, errors while validating the remote's host certificate will be ignored.
 *
 *  :proxy_url ::
 *    The url of an http proxy to use to access the remote repository.
 *
 *  :credentials ::
 *    The credentials to use for the clone operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :progress ::
 *    A callback that will be executed with the textual progress received from the remote.
 *    This is the text send over the progress side-band (ie. the "counting objects" output).
 *
 *  :transfer_progress ::
 *    A callback that will be executed to report clone progress information. It will be passed
 *    the amount of +total_objects+, +indexed_objects+, +received_objects+, +local_objects+,
 *    +total_deltas+, +indexed_deltas+, and +received_bytes+.
 *
 *  :update_tips ::
 *    A callback that will be executed each time a reference was updated locally. It will be
 *    passed the +refname+, +old_oid+ and +new_oid+.
 *
 *  Example:
 *
 *    Repository.clone_at("https://github.com/libgit2/rugged.git", "./some/dir", {
 *      transfer_progress: lambda { |total_objects, indexed_objects, received_objects, local_objects, total_deltas, indexed_deltas, received_bytes|
 *        # ...
 *      }
 *    })
 */
static VALUE rb_git_repo_clone_at(int argc, VALUE *argv, VALUE klass)
{
	VALUE url, local_path, rb_options_hash;
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	struct rugged_remote_cb_payload remote_payload = { Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, 0 };
	git_repository *repo;
	int error;

	rb_scan_args(argc, argv, "21", &url, &local_path, &rb_options_hash);
	Check_Type(url, T_STRING);
	FilePathValue(local_path);

	parse_clone_options(&options, rb_options_hash, &remote_payload);

	error = git_clone(&repo, StringValueCStr(url), StringValueCStr(local_path), &options);

	if (RTEST(remote_payload.exception))
		rb_jump_tag(remote_payload.exception);
	rugged_exception_check(error);

	return rugged_repo_new(klass, repo);
}

#define RB_GIT_REPO_OWNED_GET(_klass, _object) \
	VALUE rb_data = rb_iv_get(self, "@" #_object); \
	if (NIL_P(rb_data)) { \
		git_repository *repo; \
		git_##_object *data; \
		int error; \
		Data_Get_Struct(self, git_repository, repo); \
		error = git_repository_##_object(&data, repo); \
		rugged_exception_check(error); \
		rb_data = rugged_##_object##_new(_klass, self, data); \
		rb_iv_set(self, "@" #_object, rb_data); \
	} \
	return rb_data; \

#define RB_GIT_REPO_OWNED_SET(_klass, _object) \
	VALUE rb_old_data; \
	git_repository *repo; \
	git_##_object *data; \
	if (!rb_obj_is_kind_of(rb_data, _klass))\
		rb_raise(rb_eTypeError, \
			"The given object is not a Rugged::" #_object); \
	if (!NIL_P(rugged_owner(rb_data))) \
		rb_raise(rb_eRuntimeError, \
			"The given object is already owned by another repository"); \
	Data_Get_Struct(self, git_repository, repo); \
	Data_Get_Struct(rb_data, git_##_object, data); \
	git_repository_set_##_object(repo, data); \
	rb_old_data = rb_iv_get(self, "@" #_object); \
	if (!NIL_P(rb_old_data)) rugged_set_owner(rb_old_data, Qnil); \
	rugged_set_owner(rb_data, self); \
	rb_iv_set(self, "@" #_object, rb_data); \
	return Qnil; \


/*
 *  call-seq:
 *    repo.index = idx
 *
 *  Set the index for this +Repository+. +idx+ must be a instance of
 *  Rugged::Index. This index will be used internally by all
 *  operations that use the Git index on +repo+.
 *
 *  Note that it's not necessary to set the +index+ for any repository;
 *  by default repositories are loaded with the index file that can be
 *  located on the +.git+ folder in the filesystem.
 */
static VALUE rb_git_repo_set_index(VALUE self, VALUE rb_data)
{
	RB_GIT_REPO_OWNED_SET(rb_cRuggedIndex, index);
}

/*
 *  call-seq:
 *    repo.index -> idx
 *
 *  Return the default index for this repository.
 */
static VALUE rb_git_repo_get_index(VALUE self)
{
	RB_GIT_REPO_OWNED_GET(rb_cRuggedIndex, index);
}

/*
 *  call-seq:
 *    repo.config = cfg
 *
 *  Set the configuration file for this +Repository+. +cfg+ must be a instance of
 *  Rugged::Config. This config file will be used internally by all
 *  operations that need to lookup configuration settings on +repo+.
 *
 *  Note that it's not necessary to set the +config+ for any repository;
 *  by default repositories are loaded with their relevant config files
 *  on the filesystem, and the corresponding global and system files if
 *  they can be found.
 */
static VALUE rb_git_repo_set_config(VALUE self, VALUE rb_data)
{
	RB_GIT_REPO_OWNED_SET(rb_cRuggedConfig, config);
}

/*
 *  call-seq:
 *    repo.config -> cfg
 *
 *  Return a Rugged::Config object representing this repository's config.
 */
static VALUE rb_git_repo_get_config(VALUE self)
{
	RB_GIT_REPO_OWNED_GET(rb_cRuggedConfig, config);
}

/*
 *  call-seq:
 *    repo.ident = ident
 *
 *  Set the identity to be used for writing reflogs.
 *
 *  +ident+ can be either +nil+ or a Hash containing +name+ and/or +email+ entries.
 */
static VALUE rb_git_repo_set_ident(VALUE self, VALUE rb_ident) {
	VALUE rb_val;

	git_repository *repo;
	const char *name = NULL, *email = NULL;

	Data_Get_Struct(self, git_repository, repo);

	if (!NIL_P(rb_ident)) {
		Check_Type(rb_ident, T_HASH);

		if (!NIL_P(rb_val = rb_hash_aref(rb_ident, CSTR2SYM("name")))) {
			Check_Type(rb_val, T_STRING);
			name = StringValueCStr(rb_val);
		}

		if (!NIL_P(rb_val = rb_hash_aref(rb_ident, CSTR2SYM("email")))) {
			Check_Type(rb_val, T_STRING);
			email = StringValueCStr(rb_val);
		}
	}

	rugged_exception_check(
		git_repository_set_ident(repo, name, email)
	);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.ident -> ident
 *
 *  Return a Hash containing the identity that is used to write reflogs.
 *
 *  +ident+ is a Hash containing +name+ and/or +email+ entries, or `nil`.
 */
static VALUE rb_git_repo_get_ident(VALUE self)
{
	VALUE rb_ident = rb_hash_new();

	git_repository *repo;
	const char *name = NULL, *email = NULL;

	Data_Get_Struct(self, git_repository, repo);

	rugged_exception_check(
		git_repository_ident(&name, &email, repo)
	);

	if (name) {
		rb_hash_aset(rb_ident, CSTR2SYM("name"), rb_str_new_utf8(name));
	}

	if (email) {
		rb_hash_aset(rb_ident, CSTR2SYM("email"), rb_str_new_utf8(email));
	}

	return rb_ident;
}

/*
 *  call-seq:
 *    repo.merge_base(oid1, oid2, ...)
 *    repo.merge_base(ref1, ref2, ...)
 *    repo.merge_base(commit1, commit2, ...)
 *
 *  Find a merge base, given two or more commits or oids.
 *  Returns nil if a merge base is not found.
 */
static VALUE rb_git_repo_merge_base(VALUE self, VALUE rb_args)
{
	int error = GIT_OK, i;
	git_repository *repo;
	git_oid base, *input_array = xmalloc(sizeof(git_oid) * RARRAY_LEN(rb_args));
	int len = (int)RARRAY_LEN(rb_args);

	if (len < 2)
		rb_raise(rb_eArgError, "wrong number of arguments (%d for 2+)", len);

	Data_Get_Struct(self, git_repository, repo);

	for (i = 0; !error && i < len; ++i) {
		error = rugged_oid_get(&input_array[i], repo, rb_ary_entry(rb_args, i));
	}

	if (error) {
		xfree(input_array);
		rugged_exception_check(error);
	}

	error = git_merge_base_many(&base, repo, len, input_array);
	xfree(input_array);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_create_oid(&base);
}

/*
 *  call-seq:
 *    repo.merge_bases(oid1, oid2, ...) -> Array
 *    repo.merge_bases(ref1, ref2, ...) -> Array
 *    repo.merge_bases(commit1, commit2, ...) -> Array
 *
 *  Find all merge bases, given two or more commits or oids.
 *  Returns an empty array if no merge bases are found.
 */
static VALUE rb_git_repo_merge_bases(VALUE self, VALUE rb_args)
{
	int error = GIT_OK;
	size_t i, len = (size_t)RARRAY_LEN(rb_args);
	git_repository *repo;
	git_oidarray bases = {NULL, 0};
	git_oid *input_array;

	VALUE rb_bases;

	if (len < 2)
		rb_raise(rb_eArgError, "wrong number of arguments (%ld for 2+)", RARRAY_LEN(rb_args));

	Data_Get_Struct(self, git_repository, repo);

	input_array = xmalloc(sizeof(git_oid) * len);

	for (i = 0; !error && i < len; ++i) {
		error = rugged_oid_get(&input_array[i], repo, rb_ary_entry(rb_args, i));
	}

	if (error) {
		xfree(input_array);
		rugged_exception_check(error);
	}

	error = git_merge_bases_many(&bases, repo, len, input_array);
	xfree(input_array);

	if (error != GIT_ENOTFOUND)
		rugged_exception_check(error);

	rb_bases = rb_ary_new2(bases.count);

	for (i = 0; i < bases.count; ++i) {
		rb_ary_push(rb_bases, rugged_create_oid(&bases.ids[i]));
	}

	git_oidarray_free(&bases);

	return rb_bases;
}

/*
 *  call-seq:
 *    repo.merge_analysis(their_commit) -> Array
 *
 *  Analyzes the given commit and determines the opportunities for merging
 *  it into the repository's HEAD. Returns an Array containing a combination
 *  of the following symbols:
 *
 *  :normal ::
 *    A "normal" merge is possible, both HEAD and the given commit have
 *    diverged from their common ancestor. The divergent commits must be
 *    merged.
 *
 *  :up_to_date ::
 *    The given commit is reachable from HEAD, meaning HEAD is up-to-date
 *    and no merge needs to be performed.
 *
 *  :fastforward ::
 *    The given commit is a fast-forward from HEAD and no merge needs to be
 *    performed. HEAD can simply be set to the given commit.
 *
 *  :unborn ::
 *    The HEAD of the current repository is "unborn" and does not point to
 *    a valid commit. No merge can be performed, but the caller may wish
 *    to simply set HEAD to the given commit.
 */
static VALUE rb_git_repo_merge_analysis(int argc, VALUE *argv, VALUE self)
{
	int error;
	git_repository *repo;
	git_commit *their_commit;
	git_annotated_commit *annotated_commit;
	git_merge_analysis_t analysis;
	git_merge_preference_t preference;
	VALUE rb_their_commit, result;

	rb_scan_args(argc, argv, "10", &rb_their_commit);

	Data_Get_Struct(self, git_repository, repo);

	if (TYPE(rb_their_commit) == T_STRING) {
		rb_their_commit = rugged_object_rev_parse(self, rb_their_commit, 1);
	}

	if (!rb_obj_is_kind_of(rb_their_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	TypedData_Get_Struct(rb_their_commit, git_commit, &rugged_object_type, their_commit);

	error = git_annotated_commit_lookup(&annotated_commit, repo, git_commit_id(their_commit));
	rugged_exception_check(error);

	error = git_merge_analysis(&analysis, &preference, repo,
				   /* hack as we currently only do one commit */
				   (const git_annotated_commit **) &annotated_commit, 1);
	git_annotated_commit_free(annotated_commit);
	rugged_exception_check(error);

	result = rb_ary_new();
	if (analysis & GIT_MERGE_ANALYSIS_NORMAL)
		rb_ary_push(result, CSTR2SYM("normal"));
	if (analysis & GIT_MERGE_ANALYSIS_UP_TO_DATE)
		rb_ary_push(result, CSTR2SYM("up_to_date"));
	if (analysis & GIT_MERGE_ANALYSIS_FASTFORWARD)
		rb_ary_push(result, CSTR2SYM("fastforward"));
	if (analysis & GIT_MERGE_ANALYSIS_UNBORN)
		rb_ary_push(result, CSTR2SYM("unborn"));

	return result;
}

/*
 *  call-seq:
 *    repo.revert_commit(revert_commit, our_commit, options = {}) -> index
 *
 *	Reverts the given commit against the given "our" commit, producing an
 *	index that reflects the result of the revert.
 */
static VALUE rb_git_repo_revert_commit(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_revert_commit, rb_our_commit, rb_options;
	git_commit *revert_commit, *our_commit;
	git_index *index;
	git_repository *repo;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	unsigned int mainline = 0;
	int error;

	rb_scan_args(argc, argv, "20:", &rb_revert_commit, &rb_our_commit, &rb_options);

	if (TYPE(rb_revert_commit) == T_STRING)
		rb_revert_commit = rugged_object_rev_parse(self, rb_revert_commit, 1);

	if (TYPE(rb_our_commit) == T_STRING)
		rb_our_commit = rugged_object_rev_parse(self, rb_our_commit, 1);

	if (!rb_obj_is_kind_of(rb_revert_commit, rb_cRuggedCommit) ||
		!rb_obj_is_kind_of(rb_our_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	if (!NIL_P(rb_options)) {
		VALUE rb_mainline;

		Check_Type(rb_options, T_HASH);
		rugged_parse_merge_options(&opts, rb_options);

		rb_mainline = rb_hash_aref(rb_options, CSTR2SYM("mainline"));
		if (!NIL_P(rb_mainline)) {
			Check_Type(rb_mainline, T_FIXNUM);
			mainline = FIX2UINT(rb_mainline);
		}
	}

	Data_Get_Struct(self, git_repository, repo);
	TypedData_Get_Struct(rb_revert_commit, git_commit, &rugged_object_type, revert_commit);
	TypedData_Get_Struct(rb_our_commit, git_commit, &rugged_object_type, our_commit);

	error = git_revert_commit(&index, repo, revert_commit, our_commit, mainline, &opts);
	if (error == GIT_EMERGECONFLICT)
		return Qnil;

	rugged_exception_check(error);

	return rugged_index_new(rb_cRuggedIndex, self, index);
}

/*
 *  call-seq:
 *    repo.apply(diff, options = {}) -> true or false
 *
 *	Applies the given diff to the repository.
 *  The following options can be passed in the +options+ Hash:
 *
 *  :location ::
 *    Whether to apply the changes to the workdir (default for non-bare),
 *    the index (default for bare) or both. Valid values: +:index+, +:workdir+,
 *    +:both+.
 *
 *  :delta_callback ::
 *    While applying the patch, this callback will be executed per delta (file).
 *    The current +delta+ will be passed to the block. The block's return value
 *    determines further behavior. When the block evaluates to:
 *       - +true+: the hunk will be applied and the apply process will continue.
 *       - +false+: the hunk will be skipped, but the apply process continues.
 *       - +nil+: the hunk is not applied, and the apply process is aborted.
 *
 *  :hunk_callback ::
 *    While applying the patch, this callback will be executed per hunk.
 *    The current +hunk+ will be passed to the block. The block's return value
 *    determines further behavior, as per +:delta_callback+.
 *
 */
static VALUE rb_git_repo_apply(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_diff, rb_options;
	git_diff *diff;
	git_repository *repo;
	git_apply_options opts = GIT_APPLY_OPTIONS_INIT;
	git_apply_location_t location;
	struct rugged_apply_cb_payload payload = { Qnil, Qnil, 0 };
	int error;

	Data_Get_Struct(self, git_repository, repo);
	if (git_repository_is_bare(repo)) {
		location = GIT_APPLY_LOCATION_INDEX;
	} else {
		location = GIT_APPLY_LOCATION_WORKDIR;
	}

	rb_scan_args(argc, argv, "11", &rb_diff, &rb_options);

	if (!rb_obj_is_kind_of(rb_diff, rb_cRuggedDiff)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Diff.");
	}

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);
		rugged_parse_apply_options(&opts, &location, rb_options, &payload);
	}

	Data_Get_Struct(rb_diff, git_diff, diff);

	error = git_apply(repo, diff, location, &opts);

	rugged_exception_check(error);

	return Qtrue;
}

/*
 *  call-seq:
 *    repo.merge_commits(our_commit, their_commit, options = {}) -> index
 *
 *  Merges the two given commits, returning a Rugged::Index that reflects
 *  the result of the merge.
 *
 *  +our_commit+ and +their_commit+ can either be Rugged::Commit objects,
 *  or OIDs resolving to the former.
 */
static VALUE rb_git_repo_merge_commits(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_our_commit, rb_their_commit, rb_options;
	git_commit *our_commit, *their_commit;
	git_index *index;
	git_repository *repo;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	int error;

	rb_scan_args(argc, argv, "20:", &rb_our_commit, &rb_their_commit, &rb_options);

	if (TYPE(rb_our_commit) == T_STRING) {
		rb_our_commit = rugged_object_rev_parse(self, rb_our_commit, 1);
	}

	if (!rb_obj_is_kind_of(rb_our_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	if (TYPE(rb_their_commit) == T_STRING) {
		rb_their_commit = rugged_object_rev_parse(self, rb_their_commit, 1);
	}

	if (!rb_obj_is_kind_of(rb_their_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);
		rugged_parse_merge_options(&opts, rb_options);
	}

	Data_Get_Struct(self, git_repository, repo);
	TypedData_Get_Struct(rb_our_commit, git_commit, &rugged_object_type, our_commit);
	TypedData_Get_Struct(rb_their_commit, git_commit, &rugged_object_type, their_commit);

	error = git_merge_commits(&index, repo, our_commit, their_commit, &opts);
	if (error == GIT_EMERGECONFLICT)
		return Qnil;

	rugged_exception_check(error);

	return rugged_index_new(rb_cRuggedIndex, self, index);
}

/*
 *  call-seq:
 *    repo.include?(oid) -> true or false
 *    repo.exists?(oid) -> true or false
 *
 *  Return whether an object with the given SHA1 OID (represented as
 *  a hex string of at least 7 characters) exists in the repository.
 *
 *    repo.include?("d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f") #=> true
 *    repo.include?("d8786bfc") #=> true
 */
static VALUE rb_git_repo_exists(VALUE self, VALUE hex)
{
	git_repository *repo;
	git_odb *odb;
	git_oid oid;
	int error;

	Data_Get_Struct(self, git_repository, repo);
	Check_Type(hex, T_STRING);

	error = git_oid_fromstrn(&oid, RSTRING_PTR(hex), RSTRING_LEN(hex));
	rugged_exception_check(error);

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	error = git_odb_exists_prefix(NULL, odb, &oid, RSTRING_LEN(hex));
	git_odb_free(odb);

	if (error == 0 || error == GIT_EAMBIGUOUS)
		return Qtrue;

	return Qfalse;
}

/*
 *  call-seq:
 *    repo.read(oid) -> str
 *
 *  Read and return the raw data of the object identified by the given +oid+.
 */
static VALUE rb_git_repo_read(VALUE self, VALUE hex)
{
	git_repository *repo;
	git_oid oid;
	int error;

	Data_Get_Struct(self, git_repository, repo);
	Check_Type(hex, T_STRING);

	error = git_oid_fromstr(&oid, StringValueCStr(hex));
	rugged_exception_check(error);

	return rugged_raw_read(repo, &oid);
}

/*
 *  call-seq:
 *    repo.read_header(oid) -> hash
 *
 *  Read and return the header information in +repo+'s ODB
 *  for the object identified by the given +oid+.
 *
 *  Returns a Hash object with the following key/value pairs:
 *
 *  :type ::
 *    A Symbol denoting the object's type. Possible values are:
 *    +:tree+, +:blob+, +:commit+ or +:tag+.
 *  :len ::
 *    A Number representing the object's length, in bytes.
 */
static VALUE rb_git_repo_read_header(VALUE self, VALUE hex)
{
	git_repository *repo;
	git_oid oid;
	git_odb *odb;
	git_otype type;
	size_t len;
	VALUE rb_hash;
	int error;

	Data_Get_Struct(self, git_repository, repo);
	Check_Type(hex, T_STRING);

	error = git_oid_fromstr(&oid, StringValueCStr(hex));
	rugged_exception_check(error);

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	error = git_odb_read_header(&len, &type, odb, &oid);
	git_odb_free(odb);
	rugged_exception_check(error);

	rb_hash = rb_hash_new();
	rb_hash_aset(rb_hash, CSTR2SYM("type"), CSTR2SYM(git_object_type2string(type)));
	rb_hash_aset(rb_hash, CSTR2SYM("len"), INT2FIX(len));

	return rb_hash;
}

/**
 *  call-seq:
 *    repo.expand_oids([oid..], object_type = :any) -> hash
 *    repo.expand_oids([oid..], object_type = [type..]) -> hash
 *
 *  Expand a list of short oids to their full value, assuming they exist
 *  in the repository. If `object_type` is passed and is an array, it must
 *  be the same length as the OIDs array. If it's a single type name, all
 *  OIDs will be expected to resolve to that object type. OIDs that don't
 *  match the expected object types will not be expanded.
 *
 *  Returns a hash of `{ short_oid => full_oid }` for the short OIDs which
 *  exist in the repository and match the expected object type. Missing OIDs
 *  will not appear in the resulting hash.
 */
static VALUE rb_git_repo_expand_oids(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_result, rb_oids, rb_expected_type;

	git_repository *repo;
	git_odb *odb;
	git_odb_expand_id *expand;
	long i, expand_count;
	int error;

	Data_Get_Struct(self, git_repository, repo);
	rb_scan_args(argc, argv, "11", &rb_oids, &rb_expected_type);

	Check_Type(rb_oids, T_ARRAY);
	expand_count = RARRAY_LEN(rb_oids);
	expand = alloca(expand_count * sizeof(git_odb_expand_id));

	for (i = 0; i < expand_count; ++i) {
		VALUE rb_hex = rb_ary_entry(rb_oids, i);
		Check_Type(rb_hex, T_STRING);

		rugged_exception_check(
			git_oid_fromstrn(&expand[i].id, RSTRING_PTR(rb_hex), RSTRING_LEN(rb_hex))
		);
		expand[i].length = RSTRING_LEN(rb_hex);
	}

	if (TYPE(rb_expected_type) == T_ARRAY) {
		if (RARRAY_LEN(rb_expected_type) != expand_count)
			rb_raise(rb_eRuntimeError,
				"the `object_type` array must be the same length as the `oids` array");

		for (i = 0; i < expand_count; ++i) {
			VALUE rb_type = rb_ary_entry(rb_expected_type, i);
			expand[i].type = rugged_otype_get(rb_type);
		}
	} else {
		git_otype expected_type = GIT_OBJ_ANY;

		if (!NIL_P(rb_expected_type))
			expected_type = rugged_otype_get(rb_expected_type);

		for (i = 0; i < expand_count; ++i)
			expand[i].type = expected_type;
	}

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	error = git_odb_expand_ids(odb, expand, (size_t)expand_count);
	git_odb_free(odb);
	rugged_exception_check(error);

	rb_result = rb_hash_new();

	for (i = 0; i < expand_count; ++i) {
		if (expand[i].length) {
			rb_hash_aset(rb_result,
				rb_ary_entry(rb_oids, i), rugged_create_oid(&expand[i].id));
		}
	}

	return rb_result;
}

/*
 *  call-seq:
 *    repo.descendant_of?(commit, ancestor) -> true or false
 *
 *  +commit+ and +ancestor+ must be String commit OIDs or instances of Rugged::Commit.
 *
 *  Returns true if +commit+ is a descendant of +ancestor+, or false if not.
 */
static VALUE rb_git_repo_descendant_of(VALUE self, VALUE rb_commit, VALUE rb_ancestor)
{
	int result;
	int error;
	git_repository *repo;
	git_oid commit, ancestor;

	Data_Get_Struct(self, git_repository, repo);

	error = rugged_oid_get(&commit, repo, rb_commit);
	rugged_exception_check(error);

	error = rugged_oid_get(&ancestor, repo, rb_ancestor);
	rugged_exception_check(error);

	result = git_graph_descendant_of(repo, &commit, &ancestor);
	rugged_exception_check(result);

	return result ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    Repository.hash_data(str, type) -> oid
 *
 *  Hash the contents of +str+ as raw bytes (ignoring any encoding
 *  information) and adding the relevant header corresponding to +type+,
 *  and return a hex string representing the result from the hash.
 *
 *    Repository.hash_data('hello world', :commit) #=> "de5ba987198bcf2518885f0fc1350e5172cded78"
 *
 *    Repository.hash_data('hello_world', :tag) #=> "9d09060c850defbc7711d08b57def0d14e742f4e"
 */
static VALUE rb_git_repo_hash(VALUE self, VALUE rb_buffer, VALUE rb_type)
{
	int error;
	git_oid oid;

	Check_Type(rb_buffer, T_STRING);

	error = git_odb_hash(&oid,
		RSTRING_PTR(rb_buffer),
		RSTRING_LEN(rb_buffer),
		rugged_otype_get(rb_type)
	);
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

/*
 *  call-seq:
 *    Repository.hash_file(path, type) -> oid
 *
 *  Hash the contents of the file pointed at by +path+, assuming
 *  that it'd be stored in the ODB with the given +type+, and return
 *  a hex string representing the SHA1 OID resulting from the hash.
 *
 *    Repository.hash_file('foo.txt', :commit) #=> "de5ba987198bcf2518885f0fc1350e5172cded78"
 *
 *    Repository.hash_file('foo.txt', :tag) #=> "9d09060c850defbc7711d08b57def0d14e742f4e"
 */
static VALUE rb_git_repo_hashfile(VALUE self, VALUE rb_path, VALUE rb_type)
{
	int error;
	git_oid oid;

	FilePathValue(rb_path);

	error = git_odb_hashfile(&oid,
		StringValueCStr(rb_path),
		rugged_otype_get(rb_type)
	);
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

/*
 *  call-seq:
 *    repo.write(buffer, type) -> oid
 *
 *  Write the data contained in the +buffer+ string as a raw object of the
 *  given +type+ into the repository's object database.
 *
 *  +type+ can be either +:tag+, +:commit+, +:tree+ or +:blob+.
 *
 *  Returns the newly created object's oid.
 */
static VALUE rb_git_repo_write(VALUE self, VALUE rb_buffer, VALUE rub_type)
{
	git_repository *repo;
	git_odb_stream *stream;

	git_odb *odb;
	git_oid oid;
	int error;

	git_otype type;

	Data_Get_Struct(self, git_repository, repo);
	Check_Type(rb_buffer, T_STRING);

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	type = rugged_otype_get(rub_type);

	error = git_odb_open_wstream(&stream, odb, RSTRING_LEN(rb_buffer), type);
	git_odb_free(odb);
	rugged_exception_check(error);

	error = git_odb_stream_write(stream, RSTRING_PTR(rb_buffer), RSTRING_LEN(rb_buffer));
	if (!error)
		error = git_odb_stream_finalize_write(&oid, stream);

	git_odb_stream_free(stream);
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

#define RB_GIT_REPO_GETTER(method) \
	git_repository *repo; \
	int error; \
	Data_Get_Struct(self, git_repository, repo); \
	error = git_repository_##method(repo); \
	rugged_exception_check(error); \
	return error ? Qtrue : Qfalse; \

/*
 *  call-seq:
 *    repo.bare? -> true or false
 *
 *  Return whether a repository is bare or not. A bare repository has no
 *  working directory.
 */
static VALUE rb_git_repo_is_bare(VALUE self)
{
	RB_GIT_REPO_GETTER(is_bare);
}

/*
 *  call-seq:
 *    repo.shallow? -> true or false
 *
 *  Return whether a repository is a shallow clone or not. A shallow clone has
 *  a truncated history and can not be cloned or fetched from, nor can be
 *  pushed from nor into it.
 */
static VALUE rb_git_repo_is_shallow(VALUE self)
{
	RB_GIT_REPO_GETTER(is_shallow);
}

/*
 *  call-seq:
 *    repo.empty? -> true or false
 *
 *  Return whether a repository is empty or not. An empty repository has HEAD
 *  pointing to the default value and there are no other references.
 */
static VALUE rb_git_repo_is_empty(VALUE self)
{
	RB_GIT_REPO_GETTER(is_empty);
}

/*
 *  call-seq:
 *    repo.head_detached? -> true or false
 *
 *  Return whether the +HEAD+ of a repository is detached or not.
 */
static VALUE rb_git_repo_head_detached(VALUE self)
{
	RB_GIT_REPO_GETTER(head_detached);
}

/*
 *  call-seq:
 *    repo.head_unborn? -> true or false
 *
 *  Return whether the current branch is unborn (+HEAD+ points to a
 *  non-existent branch).
 */
static VALUE rb_git_repo_head_unborn(VALUE self)
{
	RB_GIT_REPO_GETTER(head_unborn);
}

/*
 *  call-seq:
 *    repo.head = str
 *
 *  Make the repository's +HEAD+ point to the specified reference.
 */
static VALUE rb_git_repo_set_head(VALUE self, VALUE rb_head)
{
	git_repository *repo;
	int error;

	Data_Get_Struct(self, git_repository, repo);

	Check_Type(rb_head, T_STRING);
	error = git_repository_set_head(repo, StringValueCStr(rb_head));
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.head -> ref
 *
 *  Retrieve and resolve the reference pointed at by the repository's +HEAD+.
 *
 *  Returns +nil+ if +HEAD+ is missing.
 */
static VALUE rb_git_repo_get_head(VALUE self)
{
	git_repository *repo;
	git_reference *head;
	int error;

	Data_Get_Struct(self, git_repository, repo);

	error = git_repository_head(&head, repo);
	if (error == GIT_ENOTFOUND)
		return Qnil;
	else
		rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, self, head);
}

/*
 *  call-seq:
 *    repo.path -> path
 *
 *  Return the full, normalized path to this repository. For non-bare repositories,
 *  this is the path of the actual +.git+ folder, not the working directory.
 *
 *    repo.path #=> "/home/foo/workthing/.git"
 */
static VALUE rb_git_repo_path(VALUE self)
{
	git_repository *repo;
	const char *path;

	Data_Get_Struct(self, git_repository, repo);
	path = git_repository_path(repo);

	return path ? rb_str_new_utf8(path) : Qnil;
}

/*
 *  call-seq:
 *    repo.workdir -> path or nil
 *
 *  Return the working directory for this repository, or +nil+ if
 *  the repository is bare.
 *
 *    repo1.bare? #=> false
 *    repo1.workdir #=> "/home/foo/workthing/"
 *
 *    repo2.bare? #=> true
 *    repo2.workdir #=> nil
 */
static VALUE rb_git_repo_workdir(VALUE self)
{
	git_repository *repo;
	const char *workdir;

	Data_Get_Struct(self, git_repository, repo);
	workdir = git_repository_workdir(repo);

	return workdir ? rb_str_new_utf8(workdir) : Qnil;
}

/*
 *  call-seq:
 *    repo.workdir = path
 *
 *  Sets the working directory of +repo+ to +path+. All internal
 *  operations on +repo+ that affect the working directory will
 *  instead use +path+.
 *
 *  The +workdir+ can be set on bare repositories to temporarily
 *  turn them into normal repositories.
 *
 *    repo.bare? #=> true
 *    repo.workdir = "/tmp/workdir"
 *    repo.bare? #=> false
 *    repo.checkout
 */
static VALUE rb_git_repo_set_workdir(VALUE self, VALUE rb_workdir)
{
	git_repository *repo;

	Data_Get_Struct(self, git_repository, repo);
	Check_Type(rb_workdir, T_STRING);

	rugged_exception_check(
		git_repository_set_workdir(repo, StringValueCStr(rb_workdir), 0)
	);

	return Qnil;
}

/*
 *  call-seq:
 *    Repository.discover(path = nil, across_fs = true) -> repository
 *
 *  Traverse +path+ upwards until a Git working directory with a +.git+
 *  folder has been found, open it and return it as a +Repository+
 *  object.
 *
 *  If +path+ is +nil+, the current working directory will be used as
 *  a starting point.
 *
 *  If +across_fs+ is +true+, the traversal won't stop when reaching
 *  a different device than the one that contained +path+ (only applies
 *  to UNIX-based OSses).
 */
static VALUE rb_git_repo_discover(int argc, VALUE *argv, VALUE klass)
{
	git_repository *repo;
	VALUE rb_path, rb_across_fs;
	git_buf repository_path = { NULL };
	int error, across_fs = 0;

	rb_scan_args(argc, argv, "02", &rb_path, &rb_across_fs);

	if (NIL_P(rb_path)) {
		VALUE rb_dir = rb_const_get(rb_cObject, rb_intern("Dir"));
		rb_path = rb_funcall(rb_dir, rb_intern("pwd"), 0);
	}

	if (!NIL_P(rb_across_fs)) {
		across_fs = rugged_parse_bool(rb_across_fs);
	}

	FilePathValue(rb_path);

	error = git_repository_discover(
		&repository_path,
		StringValueCStr(rb_path),
		across_fs,
		NULL
	);

	rugged_exception_check(error);

	error = git_repository_open(&repo, repository_path.ptr);
	git_buf_dispose(&repository_path);

	rugged_exception_check(error);

	return rugged_repo_new(klass, repo);
}

static VALUE flags_to_rb(unsigned int flags)
{
	VALUE rb_flags = rb_ary_new();

	if (flags & GIT_STATUS_INDEX_NEW)
		rb_ary_push(rb_flags, CSTR2SYM("index_new"));

	if (flags & GIT_STATUS_INDEX_MODIFIED)
		rb_ary_push(rb_flags, CSTR2SYM("index_modified"));

	if (flags & GIT_STATUS_INDEX_DELETED)
		rb_ary_push(rb_flags, CSTR2SYM("index_deleted"));

	if (flags & GIT_STATUS_WT_NEW)
		rb_ary_push(rb_flags, CSTR2SYM("worktree_new"));

	if (flags & GIT_STATUS_WT_MODIFIED)
		rb_ary_push(rb_flags, CSTR2SYM("worktree_modified"));

	if (flags & GIT_STATUS_WT_DELETED)
		rb_ary_push(rb_flags, CSTR2SYM("worktree_deleted"));

	if (flags & GIT_STATUS_IGNORED)
		rb_ary_push(rb_flags, CSTR2SYM("ignored"));

	return rb_flags;
}

static VALUE rb_git_repo_file_status(VALUE self, VALUE rb_path)
{
	unsigned int flags;
	int error;
	git_repository *repo;

	Data_Get_Struct(self, git_repository, repo);
	FilePathValue(rb_path);
	error = git_status_file(&flags, repo, StringValueCStr(rb_path));
	rugged_exception_check(error);

	return flags_to_rb(flags);
}

static VALUE rb_git_repo_file_each_status(VALUE self)
{
	int error, exception = 0;
	size_t i, nentries;
	git_repository *repo;
	git_status_list *list;

	Data_Get_Struct(self, git_repository, repo);

	if (!rb_block_given_p())
		rb_raise(rb_eRuntimeError,
			"A block was expected for iterating through "
			"the repository contents.");

	error = git_status_list_new(&list, repo, NULL);
	rugged_exception_check(error);

	nentries = git_status_list_entrycount(list);
	for (i = 0; i < nentries; i++) {
		const git_status_entry *entry;
		const char *path;
		VALUE args;

		entry = git_status_byindex(list, i);

		path = entry->head_to_index ?
		       entry->head_to_index->old_file.path :
		       entry->index_to_workdir->old_file.path;
		args = rb_ary_new3(2, rb_str_new_utf8(path), flags_to_rb(entry->status));
		rb_protect(rb_yield, args, &exception);
		if (exception != 0)
			break;
	}
	git_status_list_free(list);

	if (exception != 0)
		rb_jump_tag(exception);

	return Qnil;
}

static int rugged__each_id_cb(const git_oid *id, void *payload)
{
	int *exception = (int *)payload;
	rb_protect(rb_yield, rugged_create_oid(id), exception);
	return *exception ? GIT_ERROR : GIT_OK;
}

/*
 *  call-seq:
 *    repo.each_id { |id| block }
 *    repo.each_id -> Enumerator
 *
 *  Call the given +block+ once with every object ID found in +repo+
 *  and all its alternates. Object IDs are passed as 40-character
 *  strings.
 */
static VALUE rb_git_repo_each_id(VALUE self)
{
	git_repository *repo;
	git_odb *odb;
	int error, exception = 0;

	RETURN_ENUMERATOR(self, 0, 0);

	Data_Get_Struct(self, git_repository, repo);

	error = git_repository_odb(&odb, repo);
	rugged_exception_check(error);

	error = git_odb_foreach(odb, &rugged__each_id_cb, &exception);
	git_odb_free(odb);

	if (exception)
		rb_jump_tag(exception);
	rugged_exception_check(error);

	return Qnil;
}

static int parse_reset_type(VALUE rb_reset_type)
{
	ID id_reset_type;

	Check_Type(rb_reset_type, T_SYMBOL);
	id_reset_type = SYM2ID(rb_reset_type);

	if (id_reset_type == rb_intern("soft")) {
		return GIT_RESET_SOFT;
	} else if (id_reset_type == rb_intern("mixed")) {
		return GIT_RESET_MIXED;
	} else if (id_reset_type == rb_intern("hard")) {
		return GIT_RESET_HARD;
	} else {
		rb_raise(rb_eArgError,
			"Invalid reset type. Expected `:soft`, `:mixed` or `:hard`");
	}
}

/*
 *  call-seq:
 *    repo.reset(target, reset_type) -> nil
 *
 *  Sets the current head to the specified commit oid and optionally
 *  resets the index and working tree to match.
 *  - +target+: Rugged::Commit, Rugged::Tag or rev that resolves to a commit or tag object
 *  - +reset_type+: +:soft+, +:mixed+ or +:hard+
 *
 *  [:soft]
 *    the head will be moved to the commit.
 *  [:mixed]
 *    will trigger a +:soft+ reset, plus the index will be replaced
 *    with the content of the commit tree.
 *  [:hard]
 *    will trigger a +:mixed+ reset and the working directory will be
 *    replaced with the content of the index. (Untracked and ignored files
 *    will be left alone)
 *
 *  Examples:
 *
 *    repo.reset('origin/master', :hard) #=> nil
 */
static VALUE rb_git_repo_reset(VALUE self, VALUE rb_target, VALUE rb_reset_type)
{
	git_repository *repo;
	int reset_type;
	git_object *target = NULL;
	int error;

	Data_Get_Struct(self, git_repository, repo);

	reset_type = parse_reset_type(rb_reset_type);
	target = rugged_object_get(repo, rb_target, GIT_OBJ_ANY);

	error = git_reset(repo, target, reset_type, NULL);

	git_object_free(target);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.reset_path(pathspecs, target=nil) -> nil
 *
 *  Updates entries in the index from the +target+ commit tree, matching
 *  the given +pathspecs+.
 *
 *  Passing a nil +target+ will result in removing
 *  entries in the index matching the provided pathspecs.
 *
 *  - +pathspecs+: list of pathspecs to operate on (+String+ or +Array+ of +String+ objects)
 *  - +target+(optional): Rugged::Commit, Rugged::Tag or rev that resolves to a commit or tag object.
 *
 *  Examples:
 *    reset_path(File.join('subdir','file.txt'), '441034f860c1d5d90e4188d11ae0d325176869a8') #=> nil
 */
static VALUE rb_git_repo_reset_path(int argc, VALUE *argv, VALUE self)
{
	git_repository *repo;
	git_object *target = NULL;
	git_strarray pathspecs;
	VALUE rb_target, rb_paths;
	int error = 0;

	pathspecs.strings = NULL;
	pathspecs.count = 0;

	Data_Get_Struct(self, git_repository, repo);

	rb_scan_args(argc, argv, "11", &rb_paths, &rb_target);

	rugged_rb_ary_to_strarray(rb_paths, &pathspecs);

	if (!NIL_P(rb_target))
		target = rugged_object_get(repo, rb_target, GIT_OBJ_ANY);

	error = git_reset_default(repo, target, &pathspecs);

	xfree(pathspecs.strings);
	git_object_free(target);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.close -> nil
 *
 *  Frees all the resources used by this repository immediately. The repository can
 *  still be used after this call. Resources will be opened as necessary.
 *
 *  It is not required to call this method explicitly. Repositories are closed
 *  automatically before garbage collection
 */
static VALUE rb_git_repo_close(VALUE self)
{
	git_repository *repo;
	Data_Get_Struct(self, git_repository, repo);

	git_repository__cleanup(repo);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.namespace = new_namespace
 *
 *  Sets the active namespace for the repository.
 *  If set to nil, no namespace will be active.
 */
static VALUE rb_git_repo_set_namespace(VALUE self, VALUE rb_namespace)
{
	git_repository *repo;
	int error;

	Data_Get_Struct(self, git_repository, repo);

	if (!NIL_P(rb_namespace)) {
		Check_Type(rb_namespace, T_STRING);
		error = git_repository_set_namespace(repo, StringValueCStr(rb_namespace));
	} else {
		error = git_repository_set_namespace(repo, NULL);
	}
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.namespace -> str
 *
 *  Returns the active namespace for the repository.
 */
static VALUE rb_git_repo_get_namespace(VALUE self)
{
	git_repository *repo;
	const char *namespace;

	Data_Get_Struct(self, git_repository, repo);

	namespace = git_repository_get_namespace(repo);
	return namespace ? rb_str_new_utf8(namespace) : Qnil;
}

/*
 *  call-seq:
 *    repo.ahead_behind(local, upstream) -> Array
 *
 *  Returns a 2 element Array containing the number of commits
 *  that the upstream object is ahead and behind the local object.
 *
 *  +local+ and +upstream+ can either be strings containing SHA1 OIDs or
 *  Rugged::Object instances.
 */
static VALUE rb_git_repo_ahead_behind(VALUE self, VALUE rb_local, VALUE rb_upstream) {
	git_repository *repo;
	int error;
	git_oid local, upstream;
	size_t ahead, behind;
	VALUE rb_result;

	Data_Get_Struct(self, git_repository, repo);

	error = rugged_oid_get(&local, repo, rb_local);
	rugged_exception_check(error);

	error = rugged_oid_get(&upstream, repo, rb_upstream);
	rugged_exception_check(error);

	error = git_graph_ahead_behind(&ahead, &behind, repo, &local, &upstream);
	rugged_exception_check(error);

	rb_result = rb_ary_new2(2);
	rb_ary_push(rb_result, INT2FIX((int) ahead));
	rb_ary_push(rb_result, INT2FIX((int) behind));
	return rb_result;
}

/*
 *  call-seq:
 *    repo.default_signature -> signature or nil
 *
 *  Returns a +Hash+ with the default user +signature+ or +nil+.
 *
 *  Looks up the +user.name+ and +user.email+ from the configuration and
 *  uses the current time as the timestamp, and creates a new signature
 *  based on that information.  It will return +nil+ if either the
 *  +user.name+ or +user.email+ are not set.
 *
 *  Returns a +Hash+:
 *  - +:name+: the +user.name+ config value
 *  - +:email+: the +user.email+ config value
 *  - +:time+: the current time as a +Time+ instance
 */
static VALUE rb_git_repo_default_signature(VALUE self) {
	int error;
	git_repository *repo;
	git_signature *signature;
	VALUE rb_signature;

	Data_Get_Struct(self, git_repository, repo);

	error = git_signature_default(&signature, repo);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	rb_signature = rugged_signature_new(signature, NULL);
	git_signature_free(signature);
	return rb_signature;
}

void rugged__checkout_progress_cb(
	const char *path,
	size_t completed_steps,
	size_t total_steps,
	void *data
) {
	struct rugged_cb_payload *payload = data;
	VALUE args = rb_ary_new2(4);
	rb_ary_push(args, payload->rb_data);
	rb_ary_push(args, path == NULL ? Qnil : rb_str_new2(path));
	rb_ary_push(args, INT2FIX(completed_steps));
	rb_ary_push(args, INT2FIX(total_steps));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);
}

static int rugged__checkout_notify_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *data
) {
	struct rugged_cb_payload *payload = data;
	VALUE args = rb_ary_new2(5);
	rb_ary_push(args, payload->rb_data);

	switch (why) {
		case GIT_CHECKOUT_NOTIFY_CONFLICT:
			rb_ary_push(args, CSTR2SYM("conflict"));
		break;

		case GIT_CHECKOUT_NOTIFY_DIRTY:
			rb_ary_push(args, CSTR2SYM("dirty"));
		break;

		case GIT_CHECKOUT_NOTIFY_UPDATED:
			rb_ary_push(args, CSTR2SYM("updated"));
		break;

		case GIT_CHECKOUT_NOTIFY_UNTRACKED:
			rb_ary_push(args, CSTR2SYM("untracked"));
		break;

		case GIT_CHECKOUT_NOTIFY_IGNORED:
			rb_ary_push(args, CSTR2SYM("ignored"));
		break;

		default:
			rb_ary_push(args, CSTR2SYM("unknown"));
	}

	rb_ary_push(args, rb_git_delta_file_fromC(baseline));
	rb_ary_push(args, rb_git_delta_file_fromC(target));
	rb_ary_push(args, rb_git_delta_file_fromC(workdir));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

/**
 * The caller has to free the returned git_checkout_options paths strings array.
 */
void rugged_parse_checkout_options(git_checkout_options *opts, VALUE rb_options)
{
	VALUE rb_value;

	if (NIL_P(rb_options))
		return;

	Check_Type(rb_options, T_HASH);

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("progress"));
	if (!NIL_P(rb_value)) {
		struct rugged_cb_payload *payload = xmalloc(sizeof(struct rugged_cb_payload));
		payload->rb_data = rb_value;
		payload->exception = 0;

		opts->progress_payload = payload;
		opts->progress_cb = &rugged__checkout_progress_cb;
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("notify"));
	if (!NIL_P(rb_value)) {
		struct rugged_cb_payload *payload = xmalloc(sizeof(struct rugged_cb_payload));
		payload->rb_data = rb_value;
		payload->exception = 0;

		opts->notify_payload = payload;
		opts->notify_cb = &rugged__checkout_notify_cb;
	}

	if (!NIL_P(rb_value = rb_hash_aref(rb_options, CSTR2SYM("strategy")))) {
		int i;

		rb_value = rb_ary_to_ary(rb_value);
		for (i = 0; i < RARRAY_LEN(rb_value); ++i) {
			VALUE rb_strategy = rb_ary_entry(rb_value, i);

			if (rb_strategy == CSTR2SYM("safe")) {
				opts->checkout_strategy |= GIT_CHECKOUT_SAFE;
			} else if (rb_strategy == CSTR2SYM("force")) {
				opts->checkout_strategy |= GIT_CHECKOUT_FORCE;
			} else if (rb_strategy == CSTR2SYM("recreate_missing")) {
				opts->checkout_strategy |= GIT_CHECKOUT_RECREATE_MISSING;
			} else if (rb_strategy == CSTR2SYM("allow_conflicts")) {
				opts->checkout_strategy |= GIT_CHECKOUT_ALLOW_CONFLICTS;
			} else if (rb_strategy == CSTR2SYM("remove_untracked")) {
				opts->checkout_strategy |= GIT_CHECKOUT_REMOVE_UNTRACKED;
			} else if (rb_strategy == CSTR2SYM("remove_ignored")) {
				opts->checkout_strategy |= GIT_CHECKOUT_REMOVE_IGNORED;
			} else if (rb_strategy == CSTR2SYM("update_only")) {
				opts->checkout_strategy |= GIT_CHECKOUT_UPDATE_ONLY;
			} else if (rb_strategy == CSTR2SYM("dont_update_index")) {
				opts->checkout_strategy |= GIT_CHECKOUT_DONT_UPDATE_INDEX;
			} else if (rb_strategy == CSTR2SYM("no_refresh")) {
				opts->checkout_strategy |= GIT_CHECKOUT_NO_REFRESH;
			} else if (rb_strategy == CSTR2SYM("disable_pathspec_match")) {
				opts->checkout_strategy |= GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH;
			} else if (rb_strategy == CSTR2SYM("skip_locked_directories")) {
				opts->checkout_strategy |= GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES;
			} else if (rb_strategy == CSTR2SYM("skip_unmerged")) {
				opts->checkout_strategy |= GIT_CHECKOUT_SKIP_UNMERGED;
			} else if (rb_strategy == CSTR2SYM("use_ours")) {
				opts->checkout_strategy |= GIT_CHECKOUT_USE_OURS;
			} else if (rb_strategy == CSTR2SYM("use_theirs")) {
				opts->checkout_strategy |= GIT_CHECKOUT_USE_THEIRS;
			} else if (rb_strategy == CSTR2SYM("update_submodules")) {
				opts->checkout_strategy |= GIT_CHECKOUT_UPDATE_SUBMODULES;
			} else if (rb_strategy == CSTR2SYM("update_submodules_if_changed")) {
				opts->checkout_strategy |= GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED;
			} else if (rb_strategy != CSTR2SYM("none")) {
				rb_raise(rb_eArgError, "Unknown checkout strategy");
			}
		}
	}

	if (!NIL_P(rb_value = rb_hash_aref(rb_options, CSTR2SYM("notify_flags")))) {
		int i;

		rb_value = rb_ary_to_ary(rb_value);
		for (i = 0; i < RARRAY_LEN(rb_value); ++i) {
			VALUE rb_notify_flag = rb_ary_entry(rb_value, i);

			if (rb_notify_flag == CSTR2SYM("conflict")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_CONFLICT;
			} else if (rb_notify_flag == CSTR2SYM("dirty")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_DIRTY;
			} else if (rb_notify_flag == CSTR2SYM("updated")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UPDATED;
			} else if (rb_notify_flag == CSTR2SYM("untracked")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UNTRACKED;
			} else if (rb_notify_flag == CSTR2SYM("ignored")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_IGNORED;
			} else if (rb_notify_flag == CSTR2SYM("all")) {
				opts->notify_flags |= GIT_CHECKOUT_NOTIFY_ALL;
			} else if (rb_notify_flag != CSTR2SYM("none")) {
				rb_raise(rb_eArgError, "Unknown checkout notify flag");
			}
		}
	}

	opts->disable_filters = RTEST(rb_hash_aref(rb_options, CSTR2SYM("disable_filters")));

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("dir_mode"));
	if (!NIL_P(rb_value)) {
		opts->dir_mode = FIX2UINT(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("file_mode"));
	if (!NIL_P(rb_value)) {
		opts->file_mode = FIX2UINT(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("file_open_flags"));
	if (!NIL_P(rb_value)) {
		opts->file_mode = FIX2INT(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("target_directory"));
	if (!NIL_P(rb_value)) {
		opts->target_directory = StringValueCStr(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("baseline"));
	if (!NIL_P(rb_value)) {
		if (rb_obj_is_kind_of(rb_value, rb_cRuggedTree)) {
			TypedData_Get_Struct(rb_value, git_tree, &rugged_object_type, opts->baseline);
		} else {
			rb_raise(rb_eTypeError, "Expected a Rugged::Tree.");
		}
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("paths"));
	rugged_rb_ary_to_strarray(rb_value, &opts->paths);
}

/**
 *  call-seq:
 *    repo.checkout_tree(treeish[, options])
 *
 *  Updates files in the index and working tree to match the content of the
 *  tree pointed at by the +treeish+.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :progress ::
 *    A callback that will be executed for checkout progress notifications.
 *    Up to 3 parameters are passed on each execution:
 *
 *    - The path to the last updated file (or +nil+ on the very first invocation).
 *    - The number of completed checkout steps.
 *    - The number of total checkout steps to be performed.
 *
 *  :notify ::
 *    A callback that will be executed for each checkout notification types specified
 *    with +:notify_flags+. Up to 5 parameters are passed on each execution:
 *
 *    - An array containing the +:notify_flags+ that caused the callback execution.
 *    - The path of the current file.
 *    - A hash describing the baseline blob (or +nil+ if it does not exist).
 *    - A hash describing the target blob (or +nil+ if it does not exist).
 *    - A hash describing the workdir blob (or +nil+ if it does not exist).
 *
 *  :strategy ::
 *    A single symbol or an array of symbols representing the strategies to use when
 *    performing the checkout. Possible values are:
 *
 *    :none ::
 *      Perform a dry run (default).
 *
 *    :safe ::
 *      Allow safe updates that cannot overwrite uncommitted data.
 *
 *    :recreate_missing ::
 *      Allow checkout to recreate missing files.
 *
 *    :force ::
 *      Allow all updates to force working directory to look like index.
 *
 *    :allow_conflicts ::
 *      Allow checkout to make safe updates even if conflicts are found.
 *
 *    :remove_untracked ::
 *      Remove untracked files not in index (that are not ignored).
 *
 *    :remove_ignored ::
 *      Remove ignored files not in index.
 *
 *    :update_only ::
 *      Only update existing files, don't create new ones.
 *
 *    :dont_update_index ::
 *      Normally checkout updates index entries as it goes; this stops that.
 *
 *    :no_refresh ::
 *      Don't refresh index/config/etc before doing checkout.
 *
 *    :disable_pathspec_match ::
 *      Treat pathspec as simple list of exact match file paths.
 *
 *    :skip_locked_directories ::
 *      Ignore directories in use, they will be left empty.
 *
 *    :skip_unmerged ::
 *      Allow checkout to skip unmerged files (NOT IMPLEMENTED).
 *
 *    :use_ours ::
 *      For unmerged files, checkout stage 2 from index (NOT IMPLEMENTED).
 *
 *    :use_theirs ::
 *      For unmerged files, checkout stage 3 from index (NOT IMPLEMENTED).
 *
 *    :update_submodules ::
 *      Recursively checkout submodules with same options (NOT IMPLEMENTED).
 *
 *    :update_submodules_if_changed ::
 *      Recursively checkout submodules if HEAD moved in super repo (NOT IMPLEMENTED).
 *
 *  :disable_filters ::
 *    If +true+, filters like CRLF line conversion will be disabled.
 *
 *  :dir_mode ::
 *    Mode for newly created directories. Default: +0755+.
 *
 *  :file_mode ::
 *    Mode for newly created files. Default: +0755+ or +0644+.
 *
 *  :file_open_flags ::
 *    Mode for opening files. Default: <code>IO::CREAT | IO::TRUNC | IO::WRONLY</code>.
 *
 *  :notify_flags ::
 *    A single symbol or an array of symbols representing the cases in which the +:notify+
 *    callback should be invoked. Possible values are:
 *
 *    :none ::
 *      Do not invoke the +:notify+ callback (default).
 *
 *    :conflict ::
 *      Invoke the callback for conflicting paths.
 *
 *    :dirty ::
 *      Invoke the callback for "dirty" files, i.e. those that do not need an update but
 *      no longer match the baseline.
 *
 *    :updated ::
 *      Invoke the callback for any file that was changed.
 *
 *    :untracked ::
 *      Invoke the callback for untracked files.
 *
 *    :ignored ::
 *      Invoke the callback for ignored files.
 *
 *    :all ::
 *      Invoke the callback for all these cases.
 *
 *  :paths ::
 *    A glob string or an array of glob strings specifying which paths should be taken
 *    into account for the checkout operation. +nil+ will match all files.
 *    Default: +nil+.
 *
 *  :baseline ::
 *    A Rugged::Tree that represents the current, expected contents of the workdir.
 *    Default: +HEAD+.
 *
 *  :target_directory ::
 *    A path to an alternative workdir directory in which the checkout should be performed.
 */
static VALUE rb_git_checkout_tree(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_treeish, rb_options;
	git_repository *repo;
	git_object *treeish;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct rugged_cb_payload *payload;
	int error, exception = 0;

	rb_scan_args(argc, argv, "10:", &rb_treeish, &rb_options);

	if (TYPE(rb_treeish) == T_STRING) {
		rb_treeish = rugged_object_rev_parse(self, rb_treeish, 1);
	}

	if (!rb_obj_is_kind_of(rb_treeish, rb_cRuggedCommit) &&
			!rb_obj_is_kind_of(rb_treeish, rb_cRuggedTag) &&
			!rb_obj_is_kind_of(rb_treeish, rb_cRuggedTree)) {
		rb_raise(rb_eTypeError, "Expected Rugged::Commit, Rugged::Tag or Rugged::Tree");
	}

	Data_Get_Struct(self, git_repository, repo);
	TypedData_Get_Struct(rb_treeish, git_object, &rugged_object_type, treeish);

	rugged_parse_checkout_options(&opts, rb_options);

	error = git_checkout_tree(repo, treeish, &opts);
	xfree(opts.paths.strings);

	if ((payload = opts.notify_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.notify_payload);
	}

	if ((payload = opts.progress_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.progress_payload);
	}

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return Qnil;
}

/**
 *  call-seq: repo.checkout_index(index[,options]) -> nil
 *
 *  Updates files in the index and the working tree to match the content of the
 *  commit pointed at by +index+.
 *
 *  See Repository#checkout_tree for a list of supported +options+.
 */
static VALUE rb_git_checkout_index(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_index, rb_options;
	git_repository *repo;
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct rugged_cb_payload *payload;
	int error, exception = 0;

	rb_scan_args(argc, argv, "10:", &rb_index, &rb_options);

	if (!rb_obj_is_kind_of(rb_index, rb_cRuggedIndex))
		rb_raise(rb_eTypeError, "Expected Rugged::Index");

	Data_Get_Struct(self, git_repository, repo);
	Data_Get_Struct(rb_index, git_index, index);

	rugged_parse_checkout_options(&opts, rb_options);

	error = git_checkout_index(repo, index, &opts);
	xfree(opts.paths.strings);

	if ((payload = opts.notify_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.notify_payload);
	}

	if ((payload = opts.progress_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.progress_payload);
	}

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return Qnil;
}

/**
 *  call-seq: repo.checkout_head([options]) -> nil
 *
 *  Updates files in the index and the working tree to match the content of the
 *  commit pointed at by +HEAD+.
 *
 *  See Repository#checkout_tree for a list of supported +options+.
 */
static VALUE rb_git_checkout_head(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_options;
	git_repository *repo;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct rugged_cb_payload *payload;
	int error, exception = 0;

	rb_scan_args(argc, argv, "00:", &rb_options);

	Data_Get_Struct(self, git_repository, repo);

	rugged_parse_checkout_options(&opts, rb_options);

	error = git_checkout_head(repo, &opts);
	xfree(opts.paths.strings);

	if ((payload = opts.notify_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.notify_payload);
	}

	if ((payload = opts.progress_payload) != NULL) {
		exception = payload->exception;
		xfree(opts.progress_payload);
	}

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.path_ignored?(path) -> true or false
 *
 *  Return whether a path is ignored or not.
 */
static VALUE rb_git_repo_is_path_ignored(VALUE self, VALUE rb_path) {
	git_repository *repo;
	const char *path;
	int error;
	int ignored;

	Data_Get_Struct(self, git_repository, repo);
	path = StringValueCStr(rb_path);
	error = git_ignore_path_is_ignored(&ignored, repo, path);
	rugged_exception_check(error);
	return ignored ? Qtrue : Qfalse;
}

static void rugged_parse_cherrypick_options(git_cherrypick_options *opts, VALUE rb_options)
{
	VALUE rb_value;

	if (NIL_P(rb_options))
		return;

	Check_Type(rb_options, T_HASH);

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("mainline"));
	if (!NIL_P(rb_value)) {
		opts->mainline = FIX2UINT(rb_value);
	}
}

static VALUE rugged_create_attr(const char *attr)
{
	switch (git_attr_value(attr)) {
	case GIT_ATTR_TRUE_T:
		return Qtrue;

	case GIT_ATTR_FALSE_T:
		return Qfalse;

	case GIT_ATTR_VALUE_T:
		return rb_str_new2(attr);

	case GIT_ATTR_UNSPECIFIED_T:
	default:
		return Qnil;
	}
}

static int foreach_attr_hash(const char *name, const char *value, void *payload)
{
	VALUE rb_hash = (VALUE)payload;
	rb_hash_aset(rb_hash, rb_str_new2(name), rugged_create_attr(value));
	return 0;
}

static VALUE rb_git_repo_attributes(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_path, rb_names, rb_options;

	git_repository *repo;
	int error, options = 0;

	rb_scan_args(argc, argv, "12", &rb_path, &rb_names, &rb_options);

	Data_Get_Struct(self, git_repository, repo);
	FilePathValue(rb_path);

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_FIXNUM);
		options = FIX2INT(rb_options);
	}

	switch (TYPE(rb_names)) {
	case T_ARRAY:
	{
		VALUE rb_result;
		const char **values;
		const char **names;
		long i, num_attr = RARRAY_LEN(rb_names);

		if (num_attr > 32)
			rb_raise(rb_eRuntimeError, "Too many attributes requested");

		values = alloca(num_attr * sizeof(const char *));
		names = alloca(num_attr * sizeof(const char *));

		for (i = 0; i < num_attr; ++i) {
			VALUE attr = rb_ary_entry(rb_names, i);
			Check_Type(attr, T_STRING);
			names[i] = StringValueCStr(attr);
		}

		error = git_attr_get_many(
			values, repo, options,
			StringValueCStr(rb_path),
			(size_t)num_attr, names);

		rugged_exception_check(error);

		rb_result = rb_hash_new();
		for (i = 0; i < num_attr; ++i) {
			VALUE attr = rb_ary_entry(rb_names, i);
			rb_hash_aset(rb_result, attr, rugged_create_attr(values[i]));
		}
		return rb_result;
	}

	case T_STRING:
	{
		const char *value;

		error = git_attr_get(
			&value, repo, options,
			StringValueCStr(rb_path),
			StringValueCStr(rb_names));

		rugged_exception_check(error);

		return rugged_create_attr(value);
	}

	case T_NIL:
	{
		VALUE rb_result = rb_hash_new();

		error = git_attr_foreach(
			repo, options,
			StringValueCStr(rb_path),
			&foreach_attr_hash,
			(void *)rb_result);

		rugged_exception_check(error);
		return rb_result;
	}

	default:
		rb_raise(rb_eTypeError,
			"Invalid attribute name (expected String or Array)");
	}
}

/*
 *  call-seq:
 *    repo.cherrypick(commit[, options]) -> nil
 *
 *  Cherry-pick the given commit and update the index and working
 *  directory accordingly.
 *
 *  `commit` can be either a string containing a commit id or a
 *  `Rugged::Commit` object.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :mainline ::
 *    When cherry-picking a merge, you need to specify the parent number
 *    (starting from 1) which should be considered the mainline.
 */
static VALUE rb_git_repo_cherrypick(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_options, rb_commit;

	git_repository *repo;
	git_commit *commit;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	int error;

	rb_scan_args(argc, argv, "10:", &rb_commit, &rb_options);

	if (TYPE(rb_commit) == T_STRING) {
		rb_commit = rugged_object_rev_parse(self, rb_commit, 1);
	}

	if (!rb_obj_is_kind_of(rb_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	Data_Get_Struct(self, git_repository, repo);
	TypedData_Get_Struct(rb_commit, git_commit, &rugged_object_type, commit);

	rugged_parse_cherrypick_options(&opts, rb_options);

	error = git_cherrypick(repo, commit, &opts);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.cherrypick_commit(commit, our_commit, [mainline, options]) -> nil
 *
 *  Cherry-pick the given commit on the given base in-memory and
 *  return an index with the result.
 *
 *  `commit` can be either a string containing a commit id or a
 *  `Rugged::Commit` object.
 *
 *  `our_commit` is the base commit, can be either a string containing
 *  a commit id or a `Rugged::Commit` object.
 *
 *  `mainline` when cherry-picking a merge, this is the parent number
 *   (starting from 1) which should be considered the mainline.
 */
static VALUE rb_git_repo_cherrypick_commit(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_options, rb_commit, rb_our_commit, rb_mainline;

	git_repository *repo;
	git_commit *commit, *our_commit;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_index *index;
	int error, mainline;

	rb_scan_args(argc, argv, "21:", &rb_commit, &rb_our_commit, &rb_mainline, &rb_options);

	if (TYPE(rb_commit) == T_STRING) {
		rb_commit = rugged_object_rev_parse(self, rb_commit, 1);
	}
	if (TYPE(rb_our_commit) == T_STRING) {
		rb_our_commit = rugged_object_rev_parse(self, rb_our_commit, 1);
	}

	if (!rb_obj_is_kind_of(rb_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}
	if (!rb_obj_is_kind_of(rb_our_commit, rb_cRuggedCommit)) {
		rb_raise(rb_eArgError, "Expected a Rugged::Commit.");
	}

	Data_Get_Struct(self, git_repository, repo);
	TypedData_Get_Struct(rb_commit, git_commit, &rugged_object_type, commit);
	TypedData_Get_Struct(rb_our_commit, git_commit, &rugged_object_type, our_commit);

	rugged_parse_merge_options(&opts, rb_options);

	mainline = NIL_P(rb_mainline) ? 0 : FIX2UINT(rb_mainline);
	error = git_cherrypick_commit(&index, repo, commit, our_commit, mainline, &opts);
	rugged_exception_check(error);

	return rugged_index_new(rb_cRuggedIndex, self, index);
}

/*
 *  call-seq: repo.diff_from_buffer(buffer) -> Rugged::Diff object
 *
 *  Where +buffer+ is a +String+.
 *  Returns A Rugged::Diff object
 */
static VALUE rb_git_diff_from_buffer(VALUE self, VALUE rb_buffer)
{
	git_diff *diff = NULL;
	const char *buffer;
	size_t len;
	int error;

	Check_Type(rb_buffer, T_STRING);
	buffer = RSTRING_PTR(rb_buffer);
	len = RSTRING_LEN(rb_buffer);

	error = git_diff_from_buffer(&diff, buffer, len);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, self, diff);
}

void Init_rugged_repo(void)
{
	id_call = rb_intern("call");

	rb_cRuggedRepo = rb_define_class_under(rb_mRugged, "Repository", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedRepo);

	rb_define_singleton_method(rb_cRuggedRepo, "new", rb_git_repo_new, -1);
	rb_define_singleton_method(rb_cRuggedRepo, "bare", rb_git_repo_open_bare, -1);
	rb_define_singleton_method(rb_cRuggedRepo, "hash_data", rb_git_repo_hash,  2);
	rb_define_singleton_method(rb_cRuggedRepo, "hash_file", rb_git_repo_hashfile,  2);
	rb_define_singleton_method(rb_cRuggedRepo, "init_at", rb_git_repo_init_at, -1);
	rb_define_singleton_method(rb_cRuggedRepo, "discover", rb_git_repo_discover, -1);
	rb_define_singleton_method(rb_cRuggedRepo, "clone_at", rb_git_repo_clone_at, -1);

	rb_define_method(rb_cRuggedRepo, "close", rb_git_repo_close, 0);

	rb_define_method(rb_cRuggedRepo, "exists?", rb_git_repo_exists, 1);
	rb_define_method(rb_cRuggedRepo, "include?", rb_git_repo_exists, 1);
	rb_define_method(rb_cRuggedRepo, "expand_oids", rb_git_repo_expand_oids, -1);
	rb_define_method(rb_cRuggedRepo, "descendant_of?", rb_git_repo_descendant_of, 2);

	rb_define_method(rb_cRuggedRepo, "read",   rb_git_repo_read,   1);
	rb_define_method(rb_cRuggedRepo, "read_header",   rb_git_repo_read_header,   1);
	rb_define_method(rb_cRuggedRepo, "write",  rb_git_repo_write,  2);
	rb_define_method(rb_cRuggedRepo, "each_id",  rb_git_repo_each_id,  0);

	rb_define_method(rb_cRuggedRepo, "path",  rb_git_repo_path, 0);
	rb_define_method(rb_cRuggedRepo, "workdir",  rb_git_repo_workdir, 0);
	rb_define_method(rb_cRuggedRepo, "workdir=",  rb_git_repo_set_workdir, 1);
	rb_define_private_method(rb_cRuggedRepo, "file_status",  rb_git_repo_file_status, 1);
	rb_define_private_method(rb_cRuggedRepo, "each_status",  rb_git_repo_file_each_status, 0);

	rb_define_method(rb_cRuggedRepo, "index",  rb_git_repo_get_index,  0);
	rb_define_method(rb_cRuggedRepo, "index=",  rb_git_repo_set_index,  1);
	rb_define_method(rb_cRuggedRepo, "config",  rb_git_repo_get_config,  0);
	rb_define_method(rb_cRuggedRepo, "config=",  rb_git_repo_set_config,  1);

	rb_define_method(rb_cRuggedRepo, "ident", rb_git_repo_get_ident, 0);
	rb_define_method(rb_cRuggedRepo, "ident=", rb_git_repo_set_ident, 1);

	rb_define_method(rb_cRuggedRepo, "bare?",  rb_git_repo_is_bare,  0);
	rb_define_method(rb_cRuggedRepo, "shallow?",  rb_git_repo_is_shallow,  0);
	rb_define_method(rb_cRuggedRepo, "empty?",  rb_git_repo_is_empty,  0);

	rb_define_method(rb_cRuggedRepo, "head_detached?",  rb_git_repo_head_detached,  0);
	rb_define_method(rb_cRuggedRepo, "head_unborn?",  rb_git_repo_head_unborn,  0);
	rb_define_method(rb_cRuggedRepo, "head=", rb_git_repo_set_head, 1);
	rb_define_method(rb_cRuggedRepo, "head", rb_git_repo_get_head, 0);

	rb_define_method(rb_cRuggedRepo, "merge_base", rb_git_repo_merge_base, -2);
	rb_define_method(rb_cRuggedRepo, "merge_bases", rb_git_repo_merge_bases, -2);

	rb_define_method(rb_cRuggedRepo, "merge_analysis", rb_git_repo_merge_analysis, -1);
	rb_define_method(rb_cRuggedRepo, "merge_commits", rb_git_repo_merge_commits, -1);

	rb_define_method(rb_cRuggedRepo, "apply", rb_git_repo_apply, -1);

	rb_define_method(rb_cRuggedRepo, "revert_commit", rb_git_repo_revert_commit, -1);

	rb_define_method(rb_cRuggedRepo, "diff_from_buffer", rb_git_diff_from_buffer, 1);

	rb_define_method(rb_cRuggedRepo, "path_ignored?", rb_git_repo_is_path_ignored, 1);

	rb_define_method(rb_cRuggedRepo, "reset", rb_git_repo_reset, 2);
	rb_define_method(rb_cRuggedRepo, "reset_path", rb_git_repo_reset_path, -1);

	rb_define_method(rb_cRuggedRepo, "namespace=", rb_git_repo_set_namespace, 1);
	rb_define_method(rb_cRuggedRepo, "namespace", rb_git_repo_get_namespace, 0);

	rb_define_method(rb_cRuggedRepo, "ahead_behind", rb_git_repo_ahead_behind, 2);

	rb_define_method(rb_cRuggedRepo, "default_signature", rb_git_repo_default_signature, 0);

	rb_define_method(rb_cRuggedRepo, "checkout_tree", rb_git_checkout_tree, -1);
	rb_define_method(rb_cRuggedRepo, "checkout_index", rb_git_checkout_index, -1);
	rb_define_method(rb_cRuggedRepo, "checkout_head", rb_git_checkout_head, -1);

	rb_define_method(rb_cRuggedRepo, "cherrypick", rb_git_repo_cherrypick, -1);
	rb_define_method(rb_cRuggedRepo, "cherrypick_commit", rb_git_repo_cherrypick_commit, -1);
	rb_define_method(rb_cRuggedRepo, "fetch_attributes", rb_git_repo_attributes, -1);

	rb_cRuggedOdbObject = rb_define_class_under(rb_mRugged, "OdbObject", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedOdbObject);

	rb_define_method(rb_cRuggedOdbObject, "data",  rb_git_odbobj_data,  0);
	rb_define_method(rb_cRuggedOdbObject, "len",  rb_git_odbobj_size,  0);
	rb_define_method(rb_cRuggedOdbObject, "type",  rb_git_odbobj_type,  0);
	rb_define_method(rb_cRuggedOdbObject, "oid",  rb_git_odbobj_oid,  0);
}
