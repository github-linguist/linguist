/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

VALUE rb_cRuggedIndex;
extern VALUE rb_mRugged;
extern VALUE rb_cRuggedCommit;
extern VALUE rb_cRuggedDiff;
extern VALUE rb_cRuggedTree;

extern const rb_data_type_t rugged_object_type;

static void rb_git_indexentry_toC(git_index_entry *entry, VALUE rb_entry);
static VALUE rb_git_indexentry_fromC(const git_index_entry *entry);

/*
 * Index
 */

static void rb_git_index__free(git_index *index)
{
	git_index_free(index);
}

VALUE rugged_index_new(VALUE klass, VALUE owner, git_index *index)
{
	VALUE rb_index = Data_Wrap_Struct(klass, NULL, &rb_git_index__free, index);
	rugged_set_owner(rb_index, owner);
	return rb_index;
}

/*
 *  call-seq:
 *    Index.new([path])
 *
 *  Create a bare index object based on the index file at +path+.
 *
 *  Any index methods that rely on the ODB or a working directory (e.g. #add)
 *  will raise a Rugged::IndexError.
 */
static VALUE rb_git_index_new(int argc, VALUE *argv, VALUE klass)
{
	git_index *index;
	int error;

	VALUE rb_path;
	const char *path = NULL;

	if (rb_scan_args(argc, argv, "01", &rb_path) == 1) {
		Check_Type(rb_path, T_STRING);
		path = StringValueCStr(rb_path);
	}

	error = git_index_open(&index, path);
	rugged_exception_check(error);

	return rugged_index_new(klass, Qnil, index);
}

/*
 *  call-seq:
 *    index.clear -> nil
 *
 *  Clear the contents (remove all entries) of the index object. Changes are in-memory only
 *  and can be saved by calling #write.
 */
static VALUE rb_git_index_clear(VALUE self)
{
	git_index *index;
	Data_Get_Struct(self, git_index, index);
	git_index_clear(index);
	return Qnil;
}

/*
 *  call-seq:
 *    index.reload -> nil
 *
 *  Reloads the index contents from the disk, discarding any changes that
 *  have not been saved through #write.
 */
static VALUE rb_git_index_read(VALUE self)
{
	git_index *index;
	int error;

	Data_Get_Struct(self, git_index, index);

	error = git_index_read(index, 0);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.write -> nil
 *
 *  Writes the index object from memory back to the disk, persisting all changes.
 */
static VALUE rb_git_index_write(VALUE self)
{
	git_index *index;
	int error;

	Data_Get_Struct(self, git_index, index);

	error = git_index_write(index);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.count -> int
 *
 *  Returns the number of entries currently in the index.
 */
static VALUE rb_git_index_count(VALUE self)
{
	git_index *index;
	Data_Get_Struct(self, git_index, index);
	return INT2FIX(git_index_entrycount(index));
}

/*
 *  call-seq:
 *    index[path[, stage = 0]]     -> entry or nil
 *    index[position]              -> entry or nil
 *    index.get(path[, stage = 0]) -> entry or nil
 *    index.get(position)          -> entry or nil
 *
 *  Return a specific entry in the index.
 *
 *  The first two forms returns entries based on their +path+ in the index and an optional +stage+,
 *  while the last two forms return entries based on their position in the index.
 */
static VALUE rb_git_index_get(int argc, VALUE *argv, VALUE self)
{
	git_index *index;
	const git_index_entry *entry = NULL;

	VALUE rb_entry, rb_stage;

	Data_Get_Struct(self, git_index, index);

	rb_scan_args(argc, argv, "11", &rb_entry, &rb_stage);

	if (TYPE(rb_entry) == T_STRING) {
		int stage = 0;

		if (!NIL_P(rb_stage)) {
			Check_Type(rb_stage, T_FIXNUM);
			stage = FIX2INT(rb_stage);
		}

		entry = git_index_get_bypath(index, StringValueCStr(rb_entry), stage);
	}

	else if (TYPE(rb_entry) == T_FIXNUM) {
		if (argc > 1) {
			rb_raise(rb_eArgError,
				"Too many arguments when trying to lookup entry by index");
		}

		entry = git_index_get_byindex(index, FIX2INT(rb_entry));
	} else {
		rb_raise(rb_eArgError,
			"Invalid type for `entry`: expected String or Fixnum");
	}

	return entry ? rb_git_indexentry_fromC(entry) : Qnil;
}

/*
 *  call-seq:
 *    index.each { |entry| } -> nil
 *    index.each -> Enumerator
 *
 *  Passes each entry of the index to the given block.
 *
 *  If no block is given, an enumerator is returned instead.
 */
static VALUE rb_git_index_each(VALUE self)
{
	git_index *index;
	unsigned int i, count;

	RETURN_ENUMERATOR(self, 0, 0);
	Data_Get_Struct(self, git_index, index);

	count = (unsigned int)git_index_entrycount(index);
	for (i = 0; i < count; ++i) {
		const git_index_entry *entry = git_index_get_byindex(index, i);
		if (entry)
			rb_yield(rb_git_indexentry_fromC(entry));
	}

	return Qnil;
}

/*
 *  call-seq:
 *    index.remove(path[, stage = 0]) -> nil
 *
 *  Removes the entry at the given +path+ with the given +stage+
 *  from the index.
 */
static VALUE rb_git_index_remove(int argc, VALUE *argv, VALUE self)
{
	git_index *index;
	int error, stage = 0;

	VALUE rb_entry, rb_stage;

	Data_Get_Struct(self, git_index, index);

	if (rb_scan_args(argc, argv, "11", &rb_entry, &rb_stage) > 1) {
		Check_Type(rb_stage, T_FIXNUM);
		stage = FIX2INT(rb_stage);
	}

	Check_Type(rb_entry, T_STRING);

	error = git_index_remove(index, StringValueCStr(rb_entry), stage);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.remove_dir(dir[, stage = 0]) -> nil
 *
 *  Removes all entries under the given +dir+ with the given +stage+
 *  from the index.
 */
static VALUE rb_git_index_remove_directory(int argc, VALUE *argv, VALUE self)
{
	git_index *index;
	int error, stage = 0;

	VALUE rb_dir, rb_stage;

	Data_Get_Struct(self, git_index, index);

	if (rb_scan_args(argc, argv, "11", &rb_dir, &rb_stage) > 1) {
		Check_Type(rb_stage, T_FIXNUM);
		stage = FIX2INT(rb_stage);
	}

	Check_Type(rb_dir, T_STRING);

	error = git_index_remove_directory(index, StringValueCStr(rb_dir), stage);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index << entry -> nil
 *    index << path -> nil
 *    index.add(entry) -> nil
 *    index.add(path) -> nil
 *    index.update(entry) -> nil
 *    index.update(path) -> nil
 *
 *  Add a new entry to the index or update an existing entry in the index.
 *
 *  If passed a +path+ to an existing, readable file relative to the workdir,
 *  creates a new index entry based on this file.
 *
 *  Alternatively, a new index entry can be created by passing a Hash containing
 *  all key/value pairs of an index entry.
 *
 *  Any gitignore rules that might match +path+ (or the +:path+ value of the
 *  entry hash) are ignored.
 *
 *  If the index entry at +path+ (or +:path+) currently contains a merge conflict,
 *  it will no longer be marked as conflicting and the data about the conflict
 *  will be moved into the "resolve undo" (REUC) section of the index.
 */
static VALUE rb_git_index_add(VALUE self, VALUE rb_entry)
{
	git_index *index;
	int error = 0;

	Data_Get_Struct(self, git_index, index);

	if (TYPE(rb_entry) == T_HASH) {
		git_index_entry entry;

		rb_git_indexentry_toC(&entry, rb_entry);
		error = git_index_add(index, &entry);
	}

	else if (TYPE(rb_entry) == T_STRING) {
		error = git_index_add_bypath(index, StringValueCStr(rb_entry));
	}

	else {
		rb_raise(rb_eTypeError,
			"Expecting a hash defining an Index Entry or a path to a file in the repository");
	}

	rugged_exception_check(error);
	return Qnil;
}

int rugged__index_matched_path_cb(const char *path, const char *matched_pathspec, void *payload)
{
	int *exception = (int *)payload;

	VALUE rb_result, rb_args = rb_ary_new2(2);
	rb_ary_push(rb_args, rb_str_new2(path));
	rb_ary_push(rb_args, matched_pathspec == NULL ? Qnil : rb_str_new2(matched_pathspec));

	rb_result = rb_protect(rb_yield_splat, rb_args, exception);

	if (*exception)
		return GIT_ERROR;

	return RTEST(rb_result) ? 0 : 1;
}

/*
 *  call-seq:
 *    index.add_all(pathspec = [][, options])                            -> nil
 *    index.add_all(pathspec = [][, options]) { |path, pathspec| block } -> nil
 *
 *  Add or update index entries matching files in the working directory.
 *
 *  Searches the working directory for files that +pathspec+ and adds them
 *  to +index+ (by updating an existing entry or adding a new entry).
 *
 *  +pathspec+ can either be a String, or an Array of Strings.
 *  If +pathspec+ is empty, all entries in the index will be matched.
 *
 *  Files that are ignored due to +.gitignore+ rules will be skipped,
 *  unless they're already have an entry in +index+.
 *
 *  Files that are marked as the result of a merge request, will have this
 *  marking removed and the merge conflict information will be moved into the
 *  "resolve undo" (REUC) section of +index+.
 *
 *  If a block is given, each matched +path+ and the +pathspec+ that matched
 *  it will be passed to the block. If the return value of +block+ is
 *  falsy, the matching item will not be added to the index.
 *
 *  This method will fail in bare index instances.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :force ::
 *    If +true+, any +.gitignore+ rules will be ignored.
 *
 *  :disable_pathspec_match ::
 *    If +true+, glob expansion will be disabled and exact matching will be forced.
 *
 *  :check_pathspec ::
 *    If +true+, and the +:force+ options is +false+ or not given, exact matches
 *    of ignored files or files that are not already in +index+ will raise a
 *    Rugged::InvalidError. This emulates <code>git add -A</code>.
 */
static VALUE rb_git_index_add_all(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_pathspecs, rb_options;

	git_index *index;
	git_strarray pathspecs;
	int error, exception = 0;
	unsigned int flags = GIT_INDEX_ADD_DEFAULT;

	Data_Get_Struct(self, git_index, index);

	if (rb_scan_args(argc, argv, "02", &rb_pathspecs, &rb_options) > 1) {
		Check_Type(rb_options, T_HASH);

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("force"))))
			flags |= GIT_INDEX_ADD_FORCE;

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("disable_pathspec_match"))))
			flags |= GIT_INDEX_ADD_DISABLE_PATHSPEC_MATCH;

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("check_pathspec"))))
			flags |= GIT_INDEX_ADD_CHECK_PATHSPEC;
	}

	rugged_rb_ary_to_strarray(rb_pathspecs, &pathspecs);

	error = git_index_add_all(index, &pathspecs, flags,
		rb_block_given_p() ? rugged__index_matched_path_cb : NULL, &exception);

	xfree(pathspecs.strings);

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    index.update_all(pathspec = [])                            -> nil
 *    index.update_all(pathspec = []) { |path, pathspec| block } -> nil
 *
 *  Update all index entries to match the working directory.
 *
 *  Searches +index+ for entries that match +pathspec+ and synchronizes
 *  them with the content of the working directory.
 *
 *  +pathspec+ can either be a String, or an Array of Strings.
 *  If +pathspec+ is empty, all entries in the index will be matched.
 *
 *  Entries where the corresponding working directory file no longer exists
 *  get deleted, all other matched entries will get updated to reflect their
 *  working directory state (the latest version of the a file's content will
 *  automatically be added to the ODB).
 *
 *  If a block is given, each matched +path+ and the +pathspec+ that matched
 *  it will be passed to the block. If the return value of +block+ is
 *  falsy, the matching item will not be updated in the index.
 *
 *  This method will fail in bare index instances.
 */
static VALUE rb_git_index_update_all(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_pathspecs = rb_ary_new();

	git_index *index;
	git_strarray pathspecs;
	int error, exception = 0;

	Data_Get_Struct(self, git_index, index);

	rb_scan_args(argc, argv, "01", &rb_pathspecs);

	rugged_rb_ary_to_strarray(rb_pathspecs, &pathspecs);

	error = git_index_update_all(index, &pathspecs,
		rb_block_given_p() ? rugged__index_matched_path_cb : NULL, &exception);

	xfree(pathspecs.strings);

	if (exception)
		rb_jump_tag(exception);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.remove_all(pathspec = []) -> nil
 *    index.remove_all(pathspec = []) { |path, pathspec| block } -> nil
 *
 *  Remove all matching index entries.
 *
 *  Searches +index+ for entries that match +pathspec+ and removes them
 *  from the index.
 *
 *  +pathspec+ can either be a String, or an Array of Strings.
 *  If +pathspec+ is empty, all entries in the index will be matched.
 *
 *  If a block is given, each matched +path+ and the +pathspec+ that matched
 *  it will be passed to the block. If the return value of +block+ is
 *  falsy, the matching item will not be removed from the index.
 */
static VALUE rb_git_index_remove_all(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_pathspecs = rb_ary_new();

	git_index *index;
	git_strarray pathspecs;
	int error, exception = 0;

	Data_Get_Struct(self, git_index, index);

	rb_scan_args(argc, argv, "01", &rb_pathspecs);

	if (NIL_P(rb_pathspecs))
		rb_pathspecs = rb_ary_new();

	rugged_rb_ary_to_strarray(rb_ary_to_ary(rb_pathspecs), &pathspecs);

	error = git_index_remove_all(index, &pathspecs,
		rb_block_given_p() ? rugged__index_matched_path_cb : NULL, &exception);

	xfree(pathspecs.strings);

	if (exception)
		rb_jump_tag(exception);
	rugged_exception_check(error);

	return Qnil;
}

static VALUE rb_git_indexentry_fromC(const git_index_entry *entry)
{
	VALUE rb_entry, rb_mtime, rb_ctime;
	unsigned int valid, stage;

	if (!entry)
		return Qnil;

	rb_entry = rb_hash_new();

	rb_hash_aset(rb_entry, CSTR2SYM("path"), rb_str_new_utf8(entry->path));
	rb_hash_aset(rb_entry, CSTR2SYM("oid"), rugged_create_oid(&entry->id));

	rb_hash_aset(rb_entry, CSTR2SYM("dev"), INT2FIX(entry->dev));
	rb_hash_aset(rb_entry, CSTR2SYM("ino"), INT2FIX(entry->ino));
	rb_hash_aset(rb_entry, CSTR2SYM("mode"), INT2FIX(entry->mode));
	rb_hash_aset(rb_entry, CSTR2SYM("gid"), INT2FIX(entry->gid));
	rb_hash_aset(rb_entry, CSTR2SYM("uid"), INT2FIX(entry->uid));
	rb_hash_aset(rb_entry, CSTR2SYM("file_size"), INT2FIX(entry->file_size));

	valid = (entry->flags & GIT_IDXENTRY_VALID);
	rb_hash_aset(rb_entry, CSTR2SYM("valid"), valid ? Qtrue : Qfalse);

	stage = (entry->flags & GIT_IDXENTRY_STAGEMASK) >> GIT_IDXENTRY_STAGESHIFT;
	rb_hash_aset(rb_entry, CSTR2SYM("stage"), INT2FIX(stage));

	rb_mtime = rb_time_new(entry->mtime.seconds, entry->mtime.nanoseconds / 1000);
	rb_ctime = rb_time_new(entry->ctime.seconds, entry->ctime.nanoseconds / 1000);

	rb_hash_aset(rb_entry, CSTR2SYM("ctime"), rb_ctime);
	rb_hash_aset(rb_entry, CSTR2SYM("mtime"), rb_mtime);

	return rb_entry;
}

static inline uint32_t
default_entry_value(VALUE rb_entry, const char *key)
{
	VALUE val = rb_hash_aref(rb_entry, CSTR2SYM(key));
	if (NIL_P(val))
		return 0;

	Check_Type(val, T_FIXNUM);
	return FIX2INT(val);
}

static void rb_git_indexentry_toC(git_index_entry *entry, VALUE rb_entry)
{
	VALUE val;

	Check_Type(rb_entry, T_HASH);

	val = rb_hash_aref(rb_entry, CSTR2SYM("path"));
	Check_Type(val, T_STRING);
	entry->path = StringValueCStr(val);

	val = rb_hash_aref(rb_entry, CSTR2SYM("oid"));
	Check_Type(val, T_STRING);
	rugged_exception_check(
		git_oid_fromstr(&entry->id, StringValueCStr(val))
	);

	entry->dev = default_entry_value(rb_entry, "dev");
	entry->ino = default_entry_value(rb_entry, "ino");
	entry->mode = default_entry_value(rb_entry, "mode");
	entry->gid = default_entry_value(rb_entry, "gid");
	entry->uid = default_entry_value(rb_entry, "uid");
	entry->file_size = default_entry_value(rb_entry, "file_size");

	if ((val = rb_hash_aref(rb_entry, CSTR2SYM("mtime"))) != Qnil) {
		if (!rb_obj_is_kind_of(val, rb_cTime))
			rb_raise(rb_eTypeError, ":mtime must be a Time instance");

		entry->mtime.seconds = NUM2INT(rb_funcall(val, rb_intern("to_i"), 0));
		entry->mtime.nanoseconds = NUM2INT(rb_funcall(val, rb_intern("usec"), 0)) * 1000;
	} else {
		entry->mtime.seconds = entry->mtime.nanoseconds = 0;
	}

	if ((val = rb_hash_aref(rb_entry, CSTR2SYM("ctime"))) != Qnil) {
		if (!rb_obj_is_kind_of(val, rb_cTime))
			rb_raise(rb_eTypeError, ":ctime must be a Time instance");

		entry->ctime.seconds = NUM2INT(rb_funcall(val, rb_intern("to_i"), 0));
		entry->ctime.nanoseconds = NUM2INT(rb_funcall(val, rb_intern("usec"), 0)) * 1000;
	} else {
		entry->ctime.seconds = entry->ctime.nanoseconds = 0;
	}

	entry->flags = 0x0;
	entry->flags_extended = 0x0;

	val = rb_hash_aref(rb_entry, CSTR2SYM("stage"));
	if (!NIL_P(val)) {
		unsigned int stage = NUM2INT(val);
		entry->flags &= ~GIT_IDXENTRY_STAGEMASK;
		entry->flags |= (stage << GIT_IDXENTRY_STAGESHIFT) & GIT_IDXENTRY_STAGEMASK;
	}

	val = rb_hash_aref(rb_entry, CSTR2SYM("valid"));
	if (!NIL_P(val)) {
		entry->flags &= ~GIT_IDXENTRY_VALID;
		if (rugged_parse_bool(val))
			entry->flags |= GIT_IDXENTRY_VALID;
	}
}

/*
 *  call-seq:
 *    index.write_tree([repo]) -> oid
 *
 *  Write the index to a tree, either in the index's repository, or in
 *  the given +repo+.
 *
 *  If the index contains any files in conflict, writing the tree will fail.
 *
 *  Returns the OID string of the written tree object.
 */
static VALUE rb_git_index_writetree(int argc, VALUE *argv, VALUE self)
{
	git_index *index;
	git_oid tree_oid;
	int error;
	VALUE rb_repo;

	Data_Get_Struct(self, git_index, index);

	if (rb_scan_args(argc, argv, "01", &rb_repo) == 1) {
		git_repository *repo = NULL;
		rugged_check_repo(rb_repo);
		Data_Get_Struct(rb_repo, git_repository, repo);
		error = git_index_write_tree_to(&tree_oid, index, repo);
	}
	else {
		error = git_index_write_tree(&tree_oid, index);
	}

	rugged_exception_check(error);
	return rugged_create_oid(&tree_oid);
}

/*
 *  call-seq:
 *    index.read_tree(tree)
 *
 *  Clear the current index and start the index again on top of +tree+
 *
 *  Further index operations (+add+, +update+, +remove+, etc) will
 *  be considered changes on top of +tree+.
 */
static VALUE rb_git_index_readtree(VALUE self, VALUE rb_tree)
{
	git_index *index;
	git_tree *tree;
	int error;

	Data_Get_Struct(self, git_index, index);
	TypedData_Get_Struct(rb_tree, git_tree, &rugged_object_type, tree);

	if (!rb_obj_is_kind_of(rb_tree, rb_cRuggedTree)) {
		rb_raise(rb_eTypeError, "A Rugged::Tree instance is required");
	}

	error = git_index_read_tree(index, tree);
	rugged_exception_check(error);

	return Qnil;
}

static VALUE rb_git_diff_tree_to_index(VALUE self, VALUE rb_other, VALUE rb_options)
{
	git_index *index;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo;
	git_diff *diff = NULL;
	VALUE owner;
	int error;
	git_tree *other_tree;

	rugged_parse_diff_options(&opts, rb_options);

	Data_Get_Struct(self, git_index, index);
	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	// Need to flip the reverse option, so that the index is by default
	// the "old file" side of the diff.
	opts.flags ^= GIT_DIFF_REVERSE;

	TypedData_Get_Struct(rb_other, git_tree, &rugged_object_type, other_tree);
	error = git_diff_tree_to_index(&diff, repo, other_tree, index, &opts);

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, owner, diff);
}

static VALUE rb_git_diff_index_to_workdir(VALUE self, VALUE rb_options)
{
	git_index *index;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo;
	git_diff *diff = NULL;
	VALUE owner;
	int error;

	rugged_parse_diff_options(&opts, rb_options);

	Data_Get_Struct(self, git_index, index);
	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	error = git_diff_index_to_workdir(&diff, repo, index, &opts);

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, owner, diff);
}

/*
 *  call-seq:
 *    index.conflicts? -> true or false
 *
 *  Determines if the index contains entries representing conflicts.
 */
static VALUE rb_git_index_conflicts_p(VALUE self)
{
	git_index *index;
	Data_Get_Struct(self, git_index, index);
	return git_index_has_conflicts(index) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    index.conflict_add(conflict) -> nil
 *
 *  Add or update index entries that represent a conflict.
 *
 *  +conflict+ has to be a hash containing +:ancestor+, +:ours+ and
 *  +:theirs+ key/value pairs. Any of those paris can be +nil+ (or left out)
 *  to indicate that the file was not present in the respective tree during
 *  the merge.
 */
static VALUE rb_git_conflict_add(VALUE self, VALUE rb_conflict)
{
	VALUE rb_ancestor, rb_ours, rb_theirs;
	git_index *index;
	git_index_entry ancestor, ours, theirs;
	int error;

	Check_Type(rb_conflict, T_HASH);

	rb_ancestor = rb_hash_aref(rb_conflict, CSTR2SYM("ancestor"));
	rb_ours     = rb_hash_aref(rb_conflict, CSTR2SYM("ours"));
	rb_theirs   = rb_hash_aref(rb_conflict, CSTR2SYM("theirs"));

	if (!NIL_P(rb_ancestor))
		rb_git_indexentry_toC(&ancestor, rb_ancestor);

	if (!NIL_P(rb_ours))
		rb_git_indexentry_toC(&ours, rb_ours);

	if (!NIL_P(rb_theirs))
		rb_git_indexentry_toC(&theirs, rb_theirs);

	Data_Get_Struct(self, git_index, index);

	error = git_index_conflict_add(index,
		NIL_P(rb_ancestor) ? NULL : &ancestor,
		NIL_P(rb_theirs) ? NULL : &ours,
		NIL_P(rb_ours) ? NULL : &theirs);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.conflict_remove(path) -> nil
 *
 *  Removes the index entries that represent the conflict at +path+.
 */
static VALUE rb_git_conflict_remove(VALUE self, VALUE rb_path)
{
	git_index *index;
	int error;

	Check_Type(rb_path, T_STRING);

	Data_Get_Struct(self, git_index, index);

	error = git_index_conflict_remove(index, StringValueCStr(rb_path));
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    index.conflict_get(path) -> conflict or nil
 *
 *  Return index entries from the ancestor, our side and their side of
 *  the conflict at +path+.
 *
 *  If +:ancestor+, +:ours+ or +:theirs+ is +nil+, that indicates that +path+
 *  did not exist in the respective tree.
 *
 *  Returns nil if no conflict is present at +path+.
 */
static VALUE rb_git_conflict_get(VALUE self, VALUE rb_path)
{
	VALUE rb_result = rb_hash_new();
	git_index *index;
	const git_index_entry *ancestor, *ours, *theirs;
	int error;

	Check_Type(rb_path, T_STRING);

	Data_Get_Struct(self, git_index, index);

	error = git_index_conflict_get(&ancestor, &ours, &theirs, index, StringValueCStr(rb_path));
	if (error == GIT_ENOTFOUND)
		return Qnil;
	else
		rugged_exception_check(error);

	rb_hash_aset(rb_result, CSTR2SYM("ancestor"), rb_git_indexentry_fromC(ancestor));
	rb_hash_aset(rb_result, CSTR2SYM("ours"),     rb_git_indexentry_fromC(ours));
	rb_hash_aset(rb_result, CSTR2SYM("theirs"),   rb_git_indexentry_fromC(theirs));

	return rb_result;
}

/*
 *  call-seq:
 *    index.merge_file(path[, options]) -> merge_file or nil
 *    index.merge_file(path) -> merge_file or nil
 *
 *  Return merge_file (in memory) from the ancestor, our side and their side of
 *  the conflict at +path+.
 *
 *  If +:ancestor+, +:ours+ or +:theirs+ is +nil+, that indicates that +path+
 *  did not exist in the respective tree.
 *
 *  Returns nil if no conflict is present at +path+.

 *  The following options can be passed in the +options+ Hash:
 *
 *  :ancestor_label ::
 *    The name of the ancestor branch used to decorate conflict markers.
 *
 *  :our_label ::
 *    The name of our branch used to decorate conflict markers.
 *
 *  :their_label ::
 *    The name of their branch used to decorate conflict markers.
 *
 *  :favor ::
 *    Specifies how and if conflicts are auto-resolved by favoring a specific
 *    file output. Can be one of `:normal`, `:ours`, `:theirs` or `:union`.
 *    Defaults to `:normal`.
 *
 *  :style ::
 *    Specifies the type of merge file to produce. Can be one of `:standard`, `:diff3`. Defaults to `:standard`
 *
 *  :simplify ::
 *    If true, the merge file is simplified by condensing non-alphanumeric regions.
 *
 */

static VALUE rb_git_merge_file(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_path, rb_options, rb_result;
	VALUE rb_repo = rugged_owner(self);

	git_repository *repo;
	git_index *index;
	const git_index_entry *ancestor, *ours, *theirs;
	git_merge_file_result merge_file_result = {0};
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	int error;

	rb_scan_args(argc, argv, "1:", &rb_path, &rb_options);

	if (!NIL_P(rb_options))
		rugged_parse_merge_file_options(&opts, rb_options);

	Check_Type(rb_path, T_STRING);

	Data_Get_Struct(self, git_index, index);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_index_conflict_get(&ancestor, &ours, &theirs, index, StringValueCStr(rb_path));
	if (error == GIT_ENOTFOUND)
		return Qnil;
	else
		rugged_exception_check(error);

	if (ours == NULL)
		rb_raise(rb_eRuntimeError, "The conflict does not have a stage 2 entry");
	else if (theirs == NULL)
		rb_raise(rb_eRuntimeError, "The conflict does not have a stage 3 entry");

	error = git_merge_file_from_index(&merge_file_result, repo, ancestor, ours, theirs, &opts);
	rugged_exception_check(error);

	rb_result = rb_merge_file_result_fromC(&merge_file_result);
	git_merge_file_result_free(&merge_file_result);

	return rb_result;
}

/*
 *  call-seq:
 *    index.conflict_cleanup -> nil
 *
 *  Remove all conflicting entries (entries with a stage greater than 0)
 *  from the index.
 */
static VALUE rb_git_conflict_cleanup(VALUE self)
{
	git_index *index;

	Data_Get_Struct(self, git_index, index);
	git_index_conflict_cleanup(index);

	return Qnil;
}

/*
 *  call-seq:
 *    index.conflicts -> conflicts
 *
 *  Return all conflicts in +index+.
 *
 *  Each conflict is represented as a Hash with +:ancestor+, +:ours+ or
 *  +:theirs+ key-value pairs, each containing index entry data.
 *
 *  If the value of the +:ancestor+, +:ours+ or +:theirs+ key is +nil+,
 *  that indicates that file in conflict did not exists in the respective tree.
 */
static VALUE rb_git_index_conflicts(VALUE self)
{
	VALUE rb_conflicts = rb_ary_new();
	git_index *index;
	git_index_conflict_iterator *iter;
	const git_index_entry *ancestor, *ours, *theirs;
	int error;

	Data_Get_Struct(self, git_index, index);

	error = git_index_conflict_iterator_new(&iter, index);
	rugged_exception_check(error);

	while ((error = git_index_conflict_next(&ancestor, &ours, &theirs, iter)) == GIT_OK) {
		VALUE rb_conflict = rb_hash_new();

		rb_hash_aset(rb_conflict, CSTR2SYM("ancestor"), rb_git_indexentry_fromC(ancestor));
		rb_hash_aset(rb_conflict, CSTR2SYM("ours"),     rb_git_indexentry_fromC(ours));
		rb_hash_aset(rb_conflict, CSTR2SYM("theirs"),   rb_git_indexentry_fromC(theirs));

		rb_ary_push(rb_conflicts, rb_conflict);
	}

	git_index_conflict_iterator_free(iter);

	if (error != GIT_ITEROVER)
		rugged_exception_check(error);

	return rb_conflicts;
}

/*
 *  Document-class: Rugged::Index
 *
 *  == Index Entries
 *
 *  Index entries are represented as Hash instances with the following key/value pairs:
 *
 *  path: ::
 *    The entry's path in the index.
 *
 *  oid: ::
 *    The oid of the entry's git object (blob / tree).
 *
 *  dev: ::
 *    The device for the index entry.
 *
 *  ino: ::
 *    The inode for the index entry.
 *
 *  mode: ::
 *    The current permissions of the index entry.
 *
 *  gid: ::
 *    Group ID of the index entry's owner.
 *
 *  uid: ::
 *    User ID of the index entry's owner.
 *
 *  file_size: ::
 *    The index entry's size, in bytes.
 *
 *  valid: ::
 *    +true+ if the index entry is valid, +false+ otherwise.
 *
 *  stage: ::
 *    The current stage of the index entry.
 *
 *  mtime: ::
 *    A Time instance representing the index entry's time of last modification.
 *
 *  mtime: ::
 *    A Time instance representing the index entry's time of last status change
 *    (ie. change of owner, group, mode, etc.).
 */
void Init_rugged_index(void)
{
	/*
	 * Index
	 */
	rb_cRuggedIndex = rb_define_class_under(rb_mRugged, "Index", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedIndex);

	rb_define_singleton_method(rb_cRuggedIndex, "new", rb_git_index_new, -1);

	rb_define_method(rb_cRuggedIndex, "count", rb_git_index_count, 0);
	rb_define_method(rb_cRuggedIndex, "reload", rb_git_index_read, 0);
	rb_define_method(rb_cRuggedIndex, "clear", rb_git_index_clear, 0);
	rb_define_method(rb_cRuggedIndex, "write", rb_git_index_write, 0);
	rb_define_method(rb_cRuggedIndex, "get", rb_git_index_get, -1);
	rb_define_method(rb_cRuggedIndex, "[]", rb_git_index_get, -1);
	rb_define_method(rb_cRuggedIndex, "each", rb_git_index_each, 0);
	rb_define_private_method(rb_cRuggedIndex, "diff_tree_to_index", rb_git_diff_tree_to_index, 2);
	rb_define_private_method(rb_cRuggedIndex, "diff_index_to_workdir", rb_git_diff_index_to_workdir, 1);

	rb_define_method(rb_cRuggedIndex, "conflicts?", rb_git_index_conflicts_p, 0);
	rb_define_method(rb_cRuggedIndex, "conflicts", rb_git_index_conflicts, 0);
	rb_define_method(rb_cRuggedIndex, "conflict_get", rb_git_conflict_get, 1);
	rb_define_method(rb_cRuggedIndex, "conflict_add", rb_git_conflict_add, 1);
	rb_define_method(rb_cRuggedIndex, "conflict_remove", rb_git_conflict_remove, 1);
	rb_define_method(rb_cRuggedIndex, "conflict_cleanup", rb_git_conflict_cleanup, 0);

	rb_define_method(rb_cRuggedIndex, "merge_file", rb_git_merge_file, -1);

	rb_define_method(rb_cRuggedIndex, "add", rb_git_index_add, 1);
	rb_define_method(rb_cRuggedIndex, "update", rb_git_index_add, 1);
	rb_define_method(rb_cRuggedIndex, "<<", rb_git_index_add, 1);

	rb_define_method(rb_cRuggedIndex, "remove", rb_git_index_remove, -1);
	rb_define_method(rb_cRuggedIndex, "remove_dir", rb_git_index_remove_directory, -1);

	rb_define_method(rb_cRuggedIndex, "add_all", rb_git_index_add_all, -1);
	rb_define_method(rb_cRuggedIndex, "update_all", rb_git_index_update_all, -1);
	rb_define_method(rb_cRuggedIndex, "remove_all", rb_git_index_remove_all, -1);

	rb_define_method(rb_cRuggedIndex, "write_tree", rb_git_index_writetree, -1);
	rb_define_method(rb_cRuggedIndex, "read_tree", rb_git_index_readtree, 1);

	rb_const_set(rb_cRuggedIndex, rb_intern("ENTRY_FLAGS_STAGE"), INT2FIX(GIT_IDXENTRY_STAGEMASK));
	rb_const_set(rb_cRuggedIndex, rb_intern("ENTRY_FLAGS_STAGE_SHIFT"), INT2FIX(GIT_IDXENTRY_STAGESHIFT));
	rb_const_set(rb_cRuggedIndex, rb_intern("ENTRY_FLAGS_VALID"), INT2FIX(GIT_IDXENTRY_VALID));
}
