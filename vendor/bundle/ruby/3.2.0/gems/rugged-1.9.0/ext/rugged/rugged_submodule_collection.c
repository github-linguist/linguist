/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedSubmodule;
VALUE rb_cRuggedSubmoduleCollection;

/*
 *  call-seq:
 *    SubmoduleCollection.new(repo) -> submodules
 *
 *  Creates and returns a new collection of submodules for the given +repo+.
 */
static VALUE rb_git_submodule_collection_initialize(VALUE self, VALUE rb_repo)
{
	rugged_check_repo(rb_repo);
	rugged_set_owner(self, rb_repo);
	return self;
}

static void rb_git_submodule__free(git_submodule *submodule)
{
	git_submodule_free(submodule);
}

VALUE rugged_submodule_new(VALUE owner, git_submodule *submodule)
{
	VALUE rb_submodule;

	rb_submodule = Data_Wrap_Struct(
			rb_cRuggedSubmodule,
			NULL,
			&rb_git_submodule__free,
			submodule);
	rugged_set_owner(rb_submodule, owner);

	return rb_submodule;
}

/*
 *  call-seq:
 *    submodules[name] -> submodule or nil
 *
 *  Lookup +submodule+ by +name+ or +path+ (they are usually the same) in
 *  +repository+.
 *
 *  Returns +nil+ if submodule does not exist.
 *
 *  Raises <tt>Rugged::SubmoduleError</tt> if submodule exists only in working
 *  directory (i.e. there is a subdirectory that is a valid self-contained git
 *  repository) and is not mentioned in the +HEAD+, the index and the config.
 */
static VALUE rb_git_submodule_collection_aref(VALUE self, VALUE rb_name)
{
	git_repository *repo;
	git_submodule *submodule;
	int error;

	VALUE rb_repo = rugged_owner(self);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_name, T_STRING);

	error = git_submodule_lookup(
			&submodule,
			repo,
			StringValueCStr(rb_name)
		);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_submodule_new(rb_repo, submodule);
}

static int cb_submodule__each(git_submodule *submodule, const char *name, void *data)
{
	struct rugged_cb_payload *payload = data;
	git_repository *repo;
	git_submodule *dummy_sm;
	VALUE rb_repo;

	rb_repo = payload->rb_data;
	Data_Get_Struct(rb_repo, git_repository, repo);

	/* The submodule passed here has it's refcount decreased just after
	 * the foreach finishes. The only way to increase that refcount is
	 * to lookup the submodule.
	 *
	 * This should not return an error as the submodule name here is valid
	 * and exists, as it was just passed to this callback.
	 */
	git_submodule_lookup(&dummy_sm, repo, git_submodule_name(submodule));

	rb_protect(
		rb_yield,
		rugged_submodule_new(rb_repo, dummy_sm),
		&payload->exception
	);
	return (payload->exception) ? GIT_ERROR : GIT_OK;
}

/*
 *  call-seq:
 *    submodules.each { |submodule| block }
 *    submodules.each -> enumerator
 *
 *  Iterate through all the tracked submodules in the collection's +repository+.
 *
 *  The given +block+ will be called once with each +submodule+
 *  as a Rugged::Submodule instance.
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_submodule_collection_each(VALUE self)
{
	git_repository *repo;
	int error;
	struct rugged_cb_payload payload;
	VALUE rb_repo;

	RETURN_ENUMERATOR(self, 0, 0);
	rb_repo = rugged_owner(self);
	Data_Get_Struct(rb_repo, git_repository, repo);

	payload.exception = 0;
	payload.rb_data = rb_repo;

	error = git_submodule_foreach(repo, &cb_submodule__each, &payload);

	if (payload.exception)
		rb_jump_tag(payload.exception);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    submodules.setup_add(url, path[, options]) -> submodule
 *
 *  Setup a new +submodule+ for checkout in +repository+.
 *
 *  This does <tt>"git submodule add"</tt> up to the fetch and checkout of the
 *  submodule contents.  It prepares a new submodule, creates an entry in
 *  +.gitmodules+ and creates an empty initialized repository either at the
 *  given +path+ in the working directory or in +.git/modules+ with a gitlink
 *  from the working directory to the new repository.
 *
 *  To fully emulate <tt>"git submodule add"</tt> call this function, then open
 *  the submodule repository and perform the clone step as needed.
 *  Lastly, call Submodule#finalize_add to wrap up adding the new submodule and
 *  +.gitmodules+ to the index to be ready to commit.
 *
 *  - +url+: URL for the submodule's remote
 *  - +path+: path at which the submodule should be created
 *
 *  The following options can be passed in the +options+ Hash:
 *  :gitlink ::
 *    (defaults to +true+) should workdir contain a
 *    gitlink to the repository in +.git/modules+ vs. repository
 *    directly in workdir.
 *
 *  Returns the newly created +submodule+
 */
static VALUE rb_git_submodule_setup_add(int argc, VALUE *argv, VALUE self)
{
	git_submodule *submodule;
	git_repository *repo;
	int error;
	int use_gitlink = 1;
	VALUE rb_repo, rb_url, rb_path, rb_options;

	rb_scan_args(argc, argv, "20:", &rb_url, &rb_path, &rb_options);

	Check_Type(rb_url, T_STRING);
	Check_Type(rb_path, T_STRING);

	rb_repo = rugged_owner(self);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (!NIL_P(rb_options)) {
		VALUE rb_val;

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("gitlink"));
		use_gitlink = (rb_val != Qfalse);
	}

	error = git_submodule_add_setup(
			&submodule,
			repo,
			StringValueCStr(rb_url),
			StringValueCStr(rb_path),
			use_gitlink
		);

	rugged_exception_check(error);

	return rugged_submodule_new(rb_repo, submodule);
}

static git_submodule_ignore_t rb_git_subm_ignore_rule_toC(VALUE rb_ignore_rule)
{
	ID id_ignore_rule;

	Check_Type(rb_ignore_rule, T_SYMBOL);
	id_ignore_rule = SYM2ID(rb_ignore_rule);

	if (id_ignore_rule == rb_intern("none")) {
		return GIT_SUBMODULE_IGNORE_NONE;
	} else if (id_ignore_rule == rb_intern("untracked")) {
		return GIT_SUBMODULE_IGNORE_UNTRACKED;
	} else if (id_ignore_rule == rb_intern("dirty")) {
		return GIT_SUBMODULE_IGNORE_DIRTY;
	} else if (id_ignore_rule == rb_intern("all")) {
		return GIT_SUBMODULE_IGNORE_ALL;
	} else {
		rb_raise(rb_eArgError, "Invalid submodule ignore rule type.");
	}
}

static git_submodule_update_t rb_git_subm_update_rule_toC(VALUE rb_update_rule)
{
	ID id_update_rule;

	Check_Type(rb_update_rule, T_SYMBOL);
	id_update_rule = SYM2ID(rb_update_rule);

	if (id_update_rule == rb_intern("checkout")) {
		return GIT_SUBMODULE_UPDATE_CHECKOUT;
	} else if (id_update_rule == rb_intern("rebase")) {
		return GIT_SUBMODULE_UPDATE_REBASE;
	} else if (id_update_rule == rb_intern("merge")) {
		return GIT_SUBMODULE_UPDATE_MERGE;
	} else if (id_update_rule == rb_intern("none")) {
		return GIT_SUBMODULE_UPDATE_NONE;
	} else {
		rb_raise(rb_eArgError, "Invalid submodule update rule type.");
	}
}

/*
 *  call-seq:
 *    submodules.update(submodule, settings) -> nil
 *    submodules.update(name, settings) -> nil
 *
 *  Update settings for the given submodule in the submodule config.
 *
 *  Existing `Rugged::Submodule` instances are not updated, but can be
 *  reloaded by calling `#reload`.
 *
 *  The following options can be passed in the +settings+ Hash:
 *
 *  :url ::
 *    Updates the URL for the submodule.
 *
 *  :ignore_rule ::
 *    See `Rugged::Submodule#ignore_rule` for a list of accepted rules.
 *
 *  :update_rule ::
 *    See `Rugged::Submodule#update_rule` for a list of accepted rules.
 *
 *  :fetch_recurse_submodules ::
 *    Updates the +fetchRecurseSubmodules+ rule.
 */
static VALUE rb_git_submodule_update(VALUE self, VALUE rb_name_or_submodule, VALUE rb_settings)
{
	git_repository *repo;
	git_submodule_ignore_t ignore_rule = GIT_SUBMODULE_IGNORE_UNSPECIFIED;
	git_submodule_update_t update_rule = GIT_SUBMODULE_UPDATE_DEFAULT;
	const char *submodule_name;
	int fetch_recurse_submodules = 0;
	VALUE rb_repo = rugged_owner(self);
	VALUE rb_url, rb_fetch_recurse_submodules, rb_ignore_rule, rb_update_rule;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (rb_obj_is_kind_of(rb_name_or_submodule, rb_cRuggedSubmodule))
		rb_name_or_submodule = rb_funcall(rb_name_or_submodule, rb_intern("name"), 0);

	if (TYPE(rb_name_or_submodule) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Submodule instance");

	rb_url = rb_hash_aref(rb_settings, CSTR2SYM("url"));
	rb_fetch_recurse_submodules = rb_hash_aref(rb_settings, CSTR2SYM("fetch_recurse_submodules"));
	rb_ignore_rule = rb_hash_aref(rb_settings, CSTR2SYM("ignore_rule"));
	rb_update_rule = rb_hash_aref(rb_settings, CSTR2SYM("update_rule"));

	if (!NIL_P(rb_url)) {
		Check_Type(rb_url, T_STRING);
	}

	if (!NIL_P(rb_fetch_recurse_submodules)) {
		fetch_recurse_submodules = rugged_parse_bool(rb_fetch_recurse_submodules);
	}

	if (!NIL_P(rb_ignore_rule)) {
		ignore_rule = rb_git_subm_ignore_rule_toC(rb_ignore_rule);
	}

	if (!NIL_P(rb_update_rule)) {
		update_rule = rb_git_subm_update_rule_toC(rb_update_rule);
	}

	submodule_name = StringValueCStr(rb_name_or_submodule);

	if (!NIL_P(rb_url)) {
		rugged_exception_check(
			git_submodule_set_url(repo,
				submodule_name,
				StringValueCStr(rb_url)
			)
		);
	}

	if (!NIL_P(rb_fetch_recurse_submodules)) {
		rugged_exception_check(
			git_submodule_set_fetch_recurse_submodules(repo,
				submodule_name,
				fetch_recurse_submodules
			)
		);
	}

	if (!NIL_P(rb_ignore_rule)) {
		rugged_exception_check(
			git_submodule_set_ignore(repo,
				submodule_name,
				ignore_rule
			)
		);
	}

	if (!NIL_P(rb_update_rule)) {
		rugged_exception_check(
			git_submodule_set_update(repo,
				submodule_name,
				update_rule
			)
		);
	}

	return Qnil;
}

void Init_rugged_submodule_collection(void)
{
	rb_cRuggedSubmoduleCollection = rb_define_class_under(rb_mRugged, "SubmoduleCollection", rb_cObject);
	rb_include_module(rb_cRuggedSubmoduleCollection, rb_mEnumerable);

	rb_define_method(rb_cRuggedSubmoduleCollection, "initialize", rb_git_submodule_collection_initialize, 1);
	rb_define_method(rb_cRuggedSubmoduleCollection, "[]",         rb_git_submodule_collection_aref, 1);
	rb_define_method(rb_cRuggedSubmoduleCollection, "each",       rb_git_submodule_collection_each, 0);

	rb_define_method(rb_cRuggedSubmoduleCollection, "update",     rb_git_submodule_update, 2);

	rb_define_method(rb_cRuggedSubmoduleCollection, "setup_add",  rb_git_submodule_setup_add, -1);
}
