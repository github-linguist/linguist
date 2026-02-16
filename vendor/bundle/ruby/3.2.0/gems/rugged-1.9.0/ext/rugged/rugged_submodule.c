/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
extern VALUE rb_mRugged;
VALUE rb_cRuggedSubmodule;

static VALUE id_in_head, id_in_index, id_in_config, id_in_workdir;
static VALUE id_index_added, id_index_deleted, id_index_modified;
static VALUE id_wd_uninitialized, id_wd_added, id_wd_deleted, id_wd_modified;
static VALUE id_wd_index_modified, id_wd_wd_modified, id_wd_untracked;

static ID id_ignore_none, id_ignore_untracked, id_ignore_dirty, id_ignore_all;

static ID id_update_checkout, id_update_rebase, id_update_merge, id_update_none;

void init_status_list(void)
{
	id_in_head            = CSTR2SYM("in_head");
	id_in_index           = CSTR2SYM("in_index");
	id_in_config          = CSTR2SYM("in_config");
	id_in_workdir         = CSTR2SYM("in_workdir");
	id_index_added        = CSTR2SYM("added_to_index");
	id_index_deleted      = CSTR2SYM("deleted_from_index");
	id_index_modified     = CSTR2SYM("modified_in_index");
	id_wd_uninitialized   = CSTR2SYM("uninitialized");
	id_wd_added           = CSTR2SYM("added_to_workdir");
	id_wd_deleted         = CSTR2SYM("deleted_from_workdir");
	id_wd_modified        = CSTR2SYM("modified_in_workdir");
	id_wd_index_modified  = CSTR2SYM("dirty_workdir_index");
	id_wd_wd_modified     = CSTR2SYM("modified_files_in_workdir");
	id_wd_untracked       = CSTR2SYM("untracked_files_in_workdir");

	return;
}

static VALUE submodule_status_flags_to_rb(unsigned int flags)
{
	VALUE rb_flags = rb_ary_new();

	if (flags & GIT_SUBMODULE_STATUS_IN_HEAD)
		rb_ary_push(rb_flags, id_in_head);

	if (flags & GIT_SUBMODULE_STATUS_IN_INDEX)
		rb_ary_push(rb_flags, id_in_index);

	if (flags & GIT_SUBMODULE_STATUS_IN_CONFIG)
		rb_ary_push(rb_flags, id_in_config);

	if (flags & GIT_SUBMODULE_STATUS_IN_WD)
		rb_ary_push(rb_flags, id_in_workdir);

	if (flags & GIT_SUBMODULE_STATUS_INDEX_ADDED)
		rb_ary_push(rb_flags, id_index_added);

	if (flags & GIT_SUBMODULE_STATUS_INDEX_DELETED)
		rb_ary_push(rb_flags, id_index_deleted);

	if (flags & GIT_SUBMODULE_STATUS_INDEX_MODIFIED)
		rb_ary_push(rb_flags, id_index_modified);

	if (flags & GIT_SUBMODULE_STATUS_WD_UNINITIALIZED)
		rb_ary_push(rb_flags, id_wd_uninitialized);

	if (flags & GIT_SUBMODULE_STATUS_WD_ADDED)
		rb_ary_push(rb_flags, id_wd_added);

	if (flags & GIT_SUBMODULE_STATUS_WD_DELETED)
		rb_ary_push(rb_flags, id_wd_deleted);

	if (flags & GIT_SUBMODULE_STATUS_WD_MODIFIED)
		rb_ary_push(rb_flags, id_wd_modified);

	if (flags & GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED)
		rb_ary_push(rb_flags, id_wd_index_modified);

	if (flags & GIT_SUBMODULE_STATUS_WD_WD_MODIFIED)
		rb_ary_push(rb_flags, id_wd_wd_modified);

	if (flags & GIT_SUBMODULE_STATUS_WD_UNTRACKED)
		rb_ary_push(rb_flags, id_wd_untracked);

	return rb_flags;
}

/*
 *  call-seq:
 *    submodule.status -> array
 *
 *  Returns an +array+ with the status flags for a submodule.
 *
 *  Depending on the #ignore_rule property of the submodule, some of
 *  the flags may never be returned because they indicate changes that are
 *  supposed to be ignored.
 *
 *  Submodule info is contained in 4 places: the +HEAD+ tree, the index,
 *  config files (both +.git/config+ and +.gitmodules+), and the working
 *  directory. Any or all of those places might be missing information about
 *  the submodule depending on what state the repository is in. We consider
 *  all four places to build the combination of status flags.
 *
 *  There are four values that are not really status, but give basic info
 *  about what sources of submodule data are available. These will be
 *  returned even if ignore is set to +:all+.
 *
 *  :in_head ::
 *    superproject +HEAD+ contains submodule
 *  :in_index ::
 *    superproject index contains submodule
 *  :in_config ::
 *    superproject +.gitmodules+ has submodule
 *  :in_workdir ::
 *    superproject workdir has submodule
 *
 *  The following values will be returned as long as ignore is not +:all+.
 *
 *  :added_to_index ::
 *    submodule is in index, not in +HEAD+
 *  :deleted_from_index ::
 *    submodule is in +HEAD+, not in index
 *  :modified_in_index ::
 *    submodule in index and +HEAD+ don't match
 *  :uninitialized ::
 *    submodule in workdir is not initialized
 *  :added_to_workdir ::
 *    submodule is in workdir, not index
 *  :deleted_from_workdir ::
 *    submodule is in index, not workdir
 *  :modified_in_workdir ::
 *    submodule in index and workdir +HEAD+ don't match
 *
 *  The following can only be returned if ignore is +:none+ or +:untracked+.
 *
 *  :dirty_workdir_index ::
 *    submodule workdir index is dirty
 *  :modified_files_in_workdir ::
 *    submodule workdir has modified files
 *
 *  Lastly, the following will only be returned for ignore +:none+.
 *
 *  :untracked_files_in_workdir ::
 *    submodule workdir contains untracked files
 */
static VALUE rb_git_submodule_status(VALUE self)
{
	VALUE rb_repo = rugged_owner(self);
	git_submodule *submodule;
	git_repository *repo;
	unsigned int flags;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);
	Data_Get_Struct(self, git_submodule, submodule);

	rugged_exception_check(
		git_submodule_status(&flags, repo, git_submodule_name(submodule),
			GIT_SUBMODULE_IGNORE_UNSPECIFIED)
	);

	return submodule_status_flags_to_rb(flags);

}

#define RB_GIT_SUBMODULE_LOCATION_FLAG_CHECK(flag) \
	git_submodule *submodule; \
	unsigned int flags; \
	Data_Get_Struct(self, git_submodule, submodule); \
	rugged_exception_check( \
		git_submodule_location(&flags, submodule) \
	); \
	return (flags & flag) ? Qtrue : Qfalse; \

/*
 *  call-seq:
 *    submodule.in_head? -> true or false
 *
 *  Returns +true+ if superproject +HEAD+ contains submodule.
 */
static VALUE rb_git_submodule_status_in_head(VALUE self)
{
	RB_GIT_SUBMODULE_LOCATION_FLAG_CHECK(GIT_SUBMODULE_STATUS_IN_HEAD)
}

/*
 *  call-seq:
 *    submodule.in_index? -> true or false
 *
 *  Returns +true+ if superproject index contains submodule.
 */
static VALUE rb_git_submodule_status_in_index(VALUE self)
{
	RB_GIT_SUBMODULE_LOCATION_FLAG_CHECK(GIT_SUBMODULE_STATUS_IN_INDEX)
}

/*
 *  call-seq:
 *    submodule.in_config? -> true or false
 *
 *  Returns +true+ if superproject +.gitmodules+ has submodule.
 */
static VALUE rb_git_submodule_status_in_config(VALUE self)
{
	RB_GIT_SUBMODULE_LOCATION_FLAG_CHECK(GIT_SUBMODULE_STATUS_IN_CONFIG)
}

/*
 *  call-seq:
 *    submodule.in_workdir? -> true or false
 *
 *  Returns +true+ if superproject workdir has submodule.
 */
static VALUE rb_git_submodule_status_in_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_LOCATION_FLAG_CHECK(GIT_SUBMODULE_STATUS_IN_WD)
}

#define RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(flag) \
	VALUE rb_repo = rugged_owner(self); \
	git_repository *repo; \
	git_submodule *submodule; \
	unsigned int flags; \
	rugged_check_repo(rb_repo); \
	Data_Get_Struct(rb_repo, git_repository, repo); \
	Data_Get_Struct(self, git_submodule, submodule); \
	rugged_exception_check( \
		git_submodule_status(&flags, repo, git_submodule_name(submodule), \
			GIT_SUBMODULE_IGNORE_UNSPECIFIED) \
	); \
	return (flags & flag) ? Qtrue : Qfalse; \

/*
 *  call-seq:
 *    submodule.added_to_index? -> true or false
 *
 *  Returns +true+ if submodule is in index, not in +HEAD+.
 */
static VALUE rb_git_submodule_status_added_to_index(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_INDEX_ADDED)
}

/*
 *  call-seq:
 *    submodule.deleted_from_index? -> true or false
 *
 *  Returns +true+ if submodule is in +HEAD+, not in index
 */
static VALUE rb_git_submodule_status_deleted_from_index(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_INDEX_DELETED)
}

/*
 *  call-seq:
 *    submodule.modified_in_index? -> true or false
 *
 *  Returns +true+ if submodule in index and +HEAD+ don't match.
 */
static VALUE rb_git_submodule_status_modified_in_index(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_INDEX_MODIFIED)
}

/*
 *  call-seq:
 *    submodule.uninitialized? -> true or false
 *
 *  Returns +true+ if submodule in workdir is not initialized.
 */
static VALUE rb_git_submodule_status_uninitialized(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_UNINITIALIZED)
}

/*
 *  call-seq:
 *    submodule.added_to_workdir? -> true or false
 *
 *  Returns +true+ if submodule is in workdir, not index.
 */
static VALUE rb_git_submodule_status_added_to_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_ADDED)
}

/*
 *  call-seq:
 *    submodule.deleted_from_workdir? -> true or false
 *
 *  Returns +true+ if submodule is in index, not workdir.
 */
static VALUE rb_git_submodule_status_deleted_from_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_DELETED)
}

/*
 *  call-seq:
 *    submodule.modified_in_workdir? -> true or false
 *
 *  Returns +true+ if submodule in index and workdir +HEAD+ don't match.
 */
static VALUE rb_git_submodule_status_modified_in_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_MODIFIED)
}

/*
 *  call-seq:
 *    submodule.dirty_workdir_index? -> true or false
 *
 *  Returns +true+ if submodule workdir index is dirty.
 */
static VALUE rb_git_submodule_status_dirty_workdir_index(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED)
}

/*
 *  call-seq:
 *    submodule.modified_files_in_workdir? -> true or false
 *
 *  Returns +true+ if submodule workdir has modified files.
 */
static VALUE rb_git_submodule_status_modified_files_in_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_WD_MODIFIED)
}

/*
 *  call-seq:
 *    submodule.untracked_files_in_workdir? -> true or false
 *
 *  Returns +true+ if submodule workdir contains untracked files.
 */
static VALUE rb_git_submodule_status_untracked_files_in_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_FLAG_CHECK(GIT_SUBMODULE_STATUS_WD_UNTRACKED)
}

#define RB_GIT_SUBMODULE_STATUS_CHECK(check) \
	VALUE rb_repo = rugged_owner(self); \
	git_repository *repo; \
	git_submodule *submodule; \
	unsigned int flags; \
	rugged_check_repo(rb_repo); \
	Data_Get_Struct(rb_repo, git_repository, repo); \
	Data_Get_Struct(self, git_submodule, submodule); \
	rugged_exception_check( \
		git_submodule_status(&flags, repo, git_submodule_name(submodule), \
			GIT_SUBMODULE_IGNORE_UNSPECIFIED) \
	); \
	return check(flags) ? Qtrue : Qfalse; \

/*
 *  call-seq:
 *    submodule.unmodified? -> true or false
 *
 *  Returns +true+ if the submodule is unmodified.
 */
static VALUE rb_git_submodule_status_unmodified(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_CHECK(GIT_SUBMODULE_STATUS_IS_UNMODIFIED)
}

/*
 *  call-seq:
 *    submodule.dirty_workdir? -> true or false
 *
 *  Returns +true+ if the submodule workdir is dirty.
 *
 *  The workdir is considered dirty if the workdir index is modified, there
 *  are modified files in the workdir or if there are untracked files in the
 *  workdir.
 */
static VALUE rb_git_submodule_status_dirty_workdir(VALUE self)
{
	RB_GIT_SUBMODULE_STATUS_CHECK(GIT_SUBMODULE_STATUS_IS_WD_DIRTY)
}

/*
 *  call-seq:
 *    submodule.add_to_index([options]) -> submodule
 *
 *  Add current submodule +HEAD+ commit to the index of superproject.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :write_index ::
 *    (default +true+) If this should immediately write the index file.
 *    If passed as +false+, Rugged::Repository#index can be used to explicitly
 *    call Rugged::Index#write to save the change.
 */
static VALUE rb_git_submodule_add_to_index(int argc, VALUE *argv, VALUE self)
{
	git_submodule *submodule;
	VALUE rb_options;
	int write_index = 1;

	Data_Get_Struct(self, git_submodule, submodule);

	rb_scan_args(argc, argv, ":", &rb_options);

	if (!NIL_P(rb_options)) {
		VALUE rb_val;

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("write_index"));
		write_index = (rb_val != Qfalse);
	}

	rugged_exception_check(
		git_submodule_add_to_index(submodule, write_index)
	);

	return self;
}

/*
 *  call-seq:
 *    submodule.reload -> submodule
 *
 *  Reread submodule info from config, index, and +HEAD+.
 *
 *  Call this to reread cached submodule information for this submodule if
 *  there is reason to believe that it has changed.
 */
static VALUE rb_git_submodule_reload(VALUE self)
{
	git_submodule *submodule;
	Data_Get_Struct(self, git_submodule, submodule);

	rugged_exception_check(
		git_submodule_reload(submodule, 1)
	);

	return self;
}

/*
 *  call-seq:
 *    submodule.sync -> submodule
 *
 *  Copy submodule remote info into submodule repository.
 *
 *  This copies the information about the submodules URL into the checked out
 *  submodule config, acting like <tt>"git submodule sync"</tt>. This is useful
 *  if the URL for the submodule was altered (by a manual change, or a fetch of
 *  upstream changes) and the local repository needs to be updated.
 */
static VALUE rb_git_submodule_sync(VALUE self)
{
	git_submodule *submodule;
	Data_Get_Struct(self, git_submodule, submodule);

	rugged_exception_check(
		git_submodule_sync(submodule)
	);

	return self;
}

/*
 *  call-seq:
 *    submodule.init([options]) -> submodule
 *
 *  Copy submodule info into +.git/config+ file.
 *
 *  Just like <tt>"git submodule init"</tt>, this copies information about the
 *  submodule into +.git/config+.
 *
 * The following options can be passed in the +options+ Hash:
 *
 *  :overwrite ::
 *    (defaults to +false+) - By default, existing entries
 *    will not be overwritten, but setting this to +true+ forces them to be
 *    updated.
 */
static VALUE rb_git_submodule_init(int argc, VALUE *argv, VALUE self)
{
	git_submodule *submodule;
	VALUE rb_options;
	int overwrite = 0;

	Data_Get_Struct(self, git_submodule, submodule);

	rb_scan_args(argc, argv, ":", &rb_options);

	if (!NIL_P(rb_options)) {
		VALUE rb_val;

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("overwrite"));
		overwrite = RTEST(rb_val);
	}

	rugged_exception_check(
		git_submodule_init(submodule, overwrite)
	);

	return self;
}

/*
 *  call-seq:
 *    submodule.name -> string
 *
 *  Returns the name of the submodule.
 */
static VALUE rb_git_submodule_name(VALUE self)
{
	git_submodule *submodule;
	const char *name;

	Data_Get_Struct(self, git_submodule, submodule);

	name = git_submodule_name(submodule);

	return rb_str_new_utf8(name);
}

/*
 *  call-seq:
 *    submodule.url -> string or nil
 *
 *  Returns the URL of the submodule.
 */
static VALUE rb_git_submodule_url(VALUE self)
{

	git_submodule *submodule;
	const char *url;

	Data_Get_Struct(self, git_submodule, submodule);

	url = git_submodule_url(submodule);

	return url ? rb_str_new_utf8(url) : Qnil;
}

/*
 *  call-seq:
 *    submodule.path -> string
 *
 *  Returns the path of the submodule.
 *
 *  The +path+ is almost always the same as the #name,
 *  but the two are actually not required to match.
 */
static VALUE rb_git_submodule_path(VALUE self)
{
	git_submodule *submodule;
	const char *path;

	Data_Get_Struct(self, git_submodule, submodule);

	path = git_submodule_path(submodule);

	return rb_str_new_utf8(path);
}

#define RB_GIT_OID_GETTER(_klass, _attribute) \
	git_##_klass *object; \
	const git_oid * oid; \
	Data_Get_Struct(self, git_##_klass, object); \
	oid = git_##_klass##_##_attribute(object); \
	return oid ? rugged_create_oid(oid) : Qnil; \

/*
 *  call-seq:
 *    submodule.head_oid -> string or nil
 *
 *  Returns the OID for the submodule in the current +HEAD+ tree or +nil+
 *  if the submodule is not in the +HEAD+.
 */
static VALUE rb_git_submodule_head_id(VALUE self)
{
	RB_GIT_OID_GETTER(submodule, head_id);
}

/*
 *  call-seq:
 *    submodule.index_oid -> string or nil
 *
 *  Returns the OID for the submodule in the index or +nil+ if the submodule
 *  is not in the index.
 */
static VALUE rb_git_submodule_index_id(VALUE self)
{
	RB_GIT_OID_GETTER(submodule, index_id);
}

/*
 *  call-seq:
 *    submodule.workdir_oid -> string or nil
 *
 *  Returns the OID for the submodule in the current working directory or
 *  +nil+ of the submodule is not checked out.
 *
 *  This returns the OID that corresponds to looking up +HEAD+ in the checked
 *  out submodule.  If there are pending changes in the index or anything
 *  else, this won't notice that. #status can be called for a more complete
 *  picture about the state of the working directory.
 */
static VALUE rb_git_submodule_wd_id(VALUE self)
{
	RB_GIT_OID_GETTER(submodule, wd_id);
}

/*
 *  call-seq:
 *    submodule.fetch_recurse_submodules? -> true or false
 *
 *  Returns the +fetchRecurseSubmodules+ rule for a submodule.
 *
 *  This accesses the <tt>submodule.<name>.fetchRecurseSubmodules</tt> value
 *  for the submodule that controls fetching behavior for the submodule.
 *
 *  Note that at this time, +Rugged+ does not honor this setting and the fetch
 *  functionality currently ignores submodules.
 *
 */
static VALUE rb_git_submodule_fetch_recurse_submodules(VALUE self)
{
	git_submodule *submodule;
	Data_Get_Struct(self, git_submodule, submodule);

	return git_submodule_fetch_recurse_submodules(submodule) ? Qtrue : Qfalse;
}

static VALUE rb_git_subm_ignore_rule_fromC(git_submodule_ignore_t rule)
{
	switch(rule) {
	case GIT_SUBMODULE_IGNORE_NONE:
		return ID2SYM(id_ignore_none);
	case GIT_SUBMODULE_IGNORE_UNTRACKED:
		return ID2SYM(id_ignore_untracked);
	case GIT_SUBMODULE_IGNORE_DIRTY:
		return ID2SYM(id_ignore_dirty);
	case GIT_SUBMODULE_IGNORE_ALL:
		return ID2SYM(id_ignore_all);
	default:
		return CSTR2SYM("unknown");
	}
}

/*
 *  call-seq:
 *    submodule.ignore_rule -> symbol
 *
 *  Returns the ignore rule for a submodule.
 *
 *  There are four ignore values:
 *
 *  :none (default)::
 *    will consider any change to the contents of the submodule from
 *    a clean checkout to be dirty, including the addition of untracked files.
 *  :untracked ::
 *    examines the contents of the working tree but untracked files will not
 *    count as making the submodule dirty.
 *  :dirty ::
 *    means to only check if the +HEAD+ of the submodule has moved for status.
 *    This is fast since it does not need to scan the working tree
 *    of the submodule at all.
 *  :all ::
 *    means not to open the submodule repository. The working directory will be
 *    considered clean so long as there is a checked out version present.
 *
 *  See #status on how ignore rules reflect the returned status info
 *  for a submodule.
 */
static VALUE rb_git_submodule_ignore_rule(VALUE self)
{
	git_submodule *submodule;
	git_submodule_ignore_t ignore;

	Data_Get_Struct(self, git_submodule, submodule);
	ignore = git_submodule_ignore(submodule);

	return rb_git_subm_ignore_rule_fromC(ignore);
}

static VALUE rb_git_subm_update_rule_fromC(git_submodule_update_t rule)
{
	switch(rule) {
	case GIT_SUBMODULE_UPDATE_CHECKOUT:
		return ID2SYM(id_update_checkout);
	case GIT_SUBMODULE_UPDATE_REBASE:
		return ID2SYM(id_update_rebase);
	case GIT_SUBMODULE_UPDATE_MERGE:
		return ID2SYM(id_update_merge);
	case GIT_SUBMODULE_UPDATE_NONE:
		return ID2SYM(id_update_none);
	default:
		return CSTR2SYM("unknown");
	}
}

/*
 *  call-seq:
 *    submodule.update_rule -> symbol
 *
 *  Returns the update rule for a submodule.
 *
 *  There are four update_rule values:
 *
 *  :checkout (default)::
 *    the new commit specified in the superproject will be checked out in the
 *    submodule on a detached +HEAD+.
 *  :rebase ::
 *    the current branch of the submodule will be rebased onto the commit
 *    specified in the superproject.
 *  :merge ::
 *    the commit specified in the superproject will be merged into the current
 *    branch of the submodule.
 *  :none ::
 *    the submodule will not be updated
 */
static VALUE rb_git_submodule_update_rule(VALUE self)
{
	git_submodule *submodule;
	git_submodule_update_t update;

	Data_Get_Struct(self, git_submodule, submodule);
	update = git_submodule_update_strategy(submodule);

	return rb_git_subm_update_rule_fromC(update);
}

/*
 *  call-seq:
 *    submodule.repository -> repository
 *
 *  Returns the +repository+ for the submodule.
 *
 *  The returned +repository+ is a newly opened Rugged::Repository object.
 *  This will only work if the submodule is checked out into the working
 *  directory.
 */
static VALUE rb_git_submodule_repository(VALUE self)
{
	git_submodule *submodule;
	git_repository *repo;

	Data_Get_Struct(self, git_submodule, submodule);

	rugged_exception_check(
		git_submodule_open(&repo, submodule)
	);

	return rugged_repo_new(rb_cRuggedRepo, repo);
}

/*
 *  call-seq:
 *    submodule.finalize_add -> submodule
 *
 *  Resolve the setup of a new submodule.
 *
 *  This should be called on a submodule once
 *  Rugged::SubmoduleCollection#setup_add is finished and the sumodule repo is
 *  cloned.
 *
 *  This adds the +.gitmodules+ file and the newly cloned submodule to the index
 *  to be ready to be committed (but doesn't actually do the commit).
 */
static VALUE rb_git_submodule_finalize_add(VALUE self)
{
	git_submodule *submodule;
	Data_Get_Struct(self, git_submodule, submodule);

	rugged_exception_check(
		git_submodule_add_finalize(submodule)
	);

	return self;
}

void Init_rugged_submodule(void)
{
	init_status_list();
	id_ignore_none = rb_intern("none");
	id_ignore_dirty = rb_intern("dirty");
	id_ignore_untracked = rb_intern("untracked");
	id_ignore_all = rb_intern("all");

	id_update_checkout = rb_intern("checkout");
	id_update_rebase = rb_intern("rebase");
	id_update_merge = rb_intern("merge");
	id_update_none  = rb_intern("none");

	rb_cRuggedSubmodule = rb_define_class_under(rb_mRugged, "Submodule", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedSubmodule);

	rb_define_method(rb_cRuggedSubmodule, "finalize_add", rb_git_submodule_finalize_add, 0);

	rb_define_method(rb_cRuggedSubmodule, "name", rb_git_submodule_name, 0);
	rb_define_method(rb_cRuggedSubmodule, "url", rb_git_submodule_url, 0);
	rb_define_method(rb_cRuggedSubmodule, "path", rb_git_submodule_path, 0);
	rb_define_method(rb_cRuggedSubmodule, "fetch_recurse_submodules?", rb_git_submodule_fetch_recurse_submodules, 0);

	rb_define_method(rb_cRuggedSubmodule, "ignore_rule", rb_git_submodule_ignore_rule, 0);
	rb_define_method(rb_cRuggedSubmodule, "update_rule", rb_git_submodule_update_rule, 0);

	rb_define_method(rb_cRuggedSubmodule, "head_oid", rb_git_submodule_head_id, 0);
	rb_define_method(rb_cRuggedSubmodule, "index_oid", rb_git_submodule_index_id, 0);
	rb_define_method(rb_cRuggedSubmodule, "workdir_oid", rb_git_submodule_wd_id, 0);

	rb_define_method(rb_cRuggedSubmodule, "status", rb_git_submodule_status, 0);
	rb_define_method(rb_cRuggedSubmodule, "in_head?", rb_git_submodule_status_in_head, 0);
	rb_define_method(rb_cRuggedSubmodule, "in_index?", rb_git_submodule_status_in_index, 0);
	rb_define_method(rb_cRuggedSubmodule, "in_config?", rb_git_submodule_status_in_config, 0);
	rb_define_method(rb_cRuggedSubmodule, "in_workdir?", rb_git_submodule_status_in_workdir, 0);
	rb_define_method(rb_cRuggedSubmodule, "added_to_index?", rb_git_submodule_status_added_to_index, 0);
	rb_define_method(rb_cRuggedSubmodule, "deleted_from_index?", rb_git_submodule_status_deleted_from_index, 0);
	rb_define_method(rb_cRuggedSubmodule, "modified_in_index?", rb_git_submodule_status_modified_in_index, 0);
	rb_define_method(rb_cRuggedSubmodule, "uninitialized?", rb_git_submodule_status_uninitialized, 0);
	rb_define_method(rb_cRuggedSubmodule, "added_to_workdir?", rb_git_submodule_status_added_to_workdir, 0);
	rb_define_method(rb_cRuggedSubmodule, "deleted_from_workdir?", rb_git_submodule_status_deleted_from_workdir, 0);
	rb_define_method(rb_cRuggedSubmodule, "modified_in_workdir?", rb_git_submodule_status_modified_in_workdir, 0);
	rb_define_method(rb_cRuggedSubmodule, "dirty_workdir_index?", rb_git_submodule_status_dirty_workdir_index, 0);
	rb_define_method(rb_cRuggedSubmodule, "modified_files_in_workdir?", rb_git_submodule_status_modified_files_in_workdir, 0);
	rb_define_method(rb_cRuggedSubmodule, "untracked_files_in_workdir?", rb_git_submodule_status_untracked_files_in_workdir, 0);

	rb_define_method(rb_cRuggedSubmodule, "unmodified?", rb_git_submodule_status_unmodified, 0);
	rb_define_method(rb_cRuggedSubmodule, "dirty_workdir?", rb_git_submodule_status_dirty_workdir, 0);

	rb_define_method(rb_cRuggedSubmodule, "repository", rb_git_submodule_repository, 0);

	rb_define_method(rb_cRuggedSubmodule, "add_to_index", rb_git_submodule_add_to_index, -1);
	rb_define_method(rb_cRuggedSubmodule, "reload", rb_git_submodule_reload, 0);
	rb_define_method(rb_cRuggedSubmodule, "sync", rb_git_submodule_sync, 0);
	rb_define_method(rb_cRuggedSubmodule, "init", rb_git_submodule_init, -1);
}
