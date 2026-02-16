/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include <ruby/thread.h>

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedDiff;
extern VALUE rb_cRuggedIndex;
extern VALUE rb_cRuggedCommit;

VALUE rb_cRuggedTree;
VALUE rb_cRuggedTreeBuilder;

extern const rb_data_type_t rugged_object_type;

static VALUE rb_git_treeentry_fromC(const git_tree_entry *entry)
{
	VALUE rb_entry;
	VALUE type;

	if (!entry)
		return Qnil;

	rb_entry = rb_hash_new();

	rb_hash_aset(rb_entry, CSTR2SYM("name"), rb_str_new_utf8(git_tree_entry_name(entry)));
	rb_hash_aset(rb_entry, CSTR2SYM("oid"), rugged_create_oid(git_tree_entry_id(entry)));

	rb_hash_aset(rb_entry, CSTR2SYM("filemode"), INT2FIX(git_tree_entry_filemode(entry)));

	switch(git_tree_entry_type(entry)) {
		case GIT_OBJ_TREE:
			type = CSTR2SYM("tree");
			break;

		case GIT_OBJ_BLOB:
			type = CSTR2SYM("blob");
			break;

		case GIT_OBJ_COMMIT:
			type = CSTR2SYM("commit");
			break;

		default:
			type = Qnil;
			break;
	}
	rb_hash_aset(rb_entry, CSTR2SYM("type"), type);

	return rb_entry;
}

/*
 * Rugged Tree
 */

/*
 *  call-seq:
 *    tree.count -> count
 *    tree.length -> count
 *
 *  Return the number of entries contained in the tree.
 *
 *  Note that this only applies to entries in the root of the tree,
 *  not any other entries contained in sub-folders.
 */
static VALUE rb_git_tree_entrycount(VALUE self)
{
	git_tree *tree;
	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	return INT2FIX(git_tree_entrycount(tree));
}

struct rugged_treecount_cb_payload
{
	int count;
	int limit;
};

static int rugged__treecount_cb(const char *root, const git_tree_entry *entry, void *data)
{
	struct rugged_treecount_cb_payload *payload = data;

	if (payload->limit >= 0 && payload->count >= payload->limit) {
		return -1;
	} else if(git_tree_entry_type(entry) == GIT_OBJ_TREE) {
		return 0;
	} else {
		++(payload->count);
		return 1;
	}
}

/*
 *  call-seq:
 *    tree.count_recursive(limit=nil) -> count
 *
 *  `limit` - The maximum number of blobs to the count in the repository.
 *  Rugged will stop walking the tree after `limit` items to avoid long
 *  execution times.
 *
 *  Return the number of blobs (up to the limit) contained in the tree and
 *  all subtrees.
 */
static VALUE rb_git_tree_entrycount_recursive(int argc, VALUE* argv, VALUE self)
{
	git_tree *tree;
	int error;
	struct rugged_treecount_cb_payload payload;
	VALUE rb_limit;

	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	rb_scan_args(argc, argv, "01", &rb_limit);

	payload.limit = -1;
	payload.count = 0;

	if (!NIL_P(rb_limit)) {
		Check_Type(rb_limit, T_FIXNUM);
		payload.limit = FIX2INT(rb_limit);
	}


	error = git_tree_walk(tree, GIT_TREEWALK_PRE, &rugged__treecount_cb, (void *)&payload);

	if (error && giterr_last()->klass == GITERR_CALLBACK) {
		giterr_clear();
		error = 0;
	}

	rugged_exception_check(error);

	return INT2FIX(payload.count);
}

/*
 *  call-seq:
 *    tree[e] -> entry
 *    tree.get_entry(e) -> entry
 *
 *  Return one of the entries from a tree as a +Hash+. If +e+ is a number, the +e+nth entry
 *  from the tree will be returned. If +e+ is a string, the entry with that name
 *  will be returned.
 *
 *  If the entry doesn't exist, +nil+ will be returned.
 *
 *    tree[3] #=> {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *    tree['bar.txt'] #=> {:name => "bar.txt", :type => :blob, :oid => "de5ba987198bcf2518885f0fc1350e5172cded78", :filemode => 0}
 *    tree['baz.txt'] #=> nil
 */
static VALUE rb_git_tree_get_entry(VALUE self, VALUE entry_id)
{
	git_tree *tree;
	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	if (TYPE(entry_id) == T_FIXNUM)
		return rb_git_treeentry_fromC(git_tree_entry_byindex(tree, FIX2INT(entry_id)));

	else if (TYPE(entry_id) == T_STRING)
		return rb_git_treeentry_fromC(git_tree_entry_byname(tree, StringValueCStr(entry_id)));

	else
		rb_raise(rb_eTypeError, "entry_id must be either an index or a filename");
}

/*
 *  call-seq:
 *    tree.get_entry_by_oid(rb_oid) -> entry
 *
 *  Return one of the entries from a tree as a +Hash+, based off the oid SHA.
 *
 *  If the entry doesn't exist, +nil+ will be returned.
 *
 *  This does a full traversal of the every element in the tree, so this method
 *  is not especially fast.
 *
 *    tree.get_entry_by_oid("d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f")
 *    #=> {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *
 */
static VALUE rb_git_tree_get_entry_by_oid(VALUE self, VALUE rb_oid)
{
	git_tree *tree;
	git_oid oid;
	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	Check_Type(rb_oid, T_STRING);
	rugged_exception_check(git_oid_fromstr(&oid, StringValueCStr(rb_oid)));

	return rb_git_treeentry_fromC(git_tree_entry_byid(tree, &oid));
}

/*
 *  call-seq:
 *    tree.each { |entry| block }
 *    tree.each -> enumerator
 *
 *  Call +block+ with each of the entries of the subtree as a +Hash+. If no +block+
 *  is given, an +enumerator+ is returned instead.
 *
 *  Note that only the entries in the root of the tree are yielded; if you need to
 *  list also entries in subfolders, use +tree.walk+ instead.
 *
 *    tree.each { |entry| puts entry.inspect }
 *
 *  generates:
 *
 *    {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *    {:name => "bar.txt", :type => :blob, :oid => "de5ba987198bcf2518885f0fc1350e5172cded78", :filemode => 0}
 *    ...
 */
static VALUE rb_git_tree_each(VALUE self)
{
	git_tree *tree;
	size_t i, count;

	RETURN_ENUMERATOR(self, 0, 0);
	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	count = git_tree_entrycount(tree);

	for (i = 0; i < count; ++i) {
		const git_tree_entry *entry = git_tree_entry_byindex(tree, i);
		rb_yield(rb_git_treeentry_fromC(entry));
	}

	return Qnil;
}

static int rugged__treewalk_cb(const char *root, const git_tree_entry *entry, void *payload)
{
	int *exception = (int *)payload;

	VALUE rb_result, rb_args = rb_ary_new2(2);

	rb_ary_push(rb_args, rb_str_new_utf8(root));
	rb_ary_push(rb_args, rb_git_treeentry_fromC(entry));

	rb_result = rb_protect(rb_yield_splat, rb_args, exception);

	if (*exception)
		return -1;

	/* skip entry when 'false' is returned */
	if (TYPE(rb_result) == T_FALSE)
		return 1;

	/* otherwise continue normal iteration */
	return 0;
}

/*
 *  call-seq:
 *    tree.walk(mode) { |root, entry| block }
 *    tree.walk(mode) -> Enumerator
 *
 *  Walk +tree+ with the given mode (either +:preorder+ or +:postorder+) and yield
 *  to +block+ every entry in +tree+ and all its subtrees, as a +Hash+. The +block+
 *  also takes a +root+, the relative path in the traversal, starting from the root
 *  of the original tree.
 *
 *  If the +block+ returns a falsy value, that entry and its sub-entries (in the case
 *  of a folder) will be skipped for the iteration.
 *
 *  If no +block+ is given, an +Enumerator+ is returned instead.
 *
 *    tree.walk(:postorder) { |root, entry| puts "#{root}#{entry[:name]} [#{entry[:oid]}]" }
 *
 *  generates:
 *
 *    USAGE.rb [02bae86c91f96b5fdb6b1cf06f5aa3612139e318]
 *    ext [23f135b3c576b6ac4785821888991d7089f35db1]
 *    ext/rugged [25c88faa9302e34e16664eb9c990deb2bcf77849]
 *    ext/rugged/extconf.rb [40c1aa8a8cec8ca444ed5758e3f00ecff093070a]
 *    ...
 */
static VALUE rb_git_tree_walk(VALUE self, VALUE rb_mode)
{
	git_tree *tree;
	int error, mode = 0, exception = 0;
	ID id_mode;

	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);

	if (!rb_block_given_p())
		return rb_funcall(self, rb_intern("to_enum"), 2, CSTR2SYM("walk"), rb_mode);

	Check_Type(rb_mode, T_SYMBOL);
	id_mode = SYM2ID(rb_mode);

	if (id_mode == rb_intern("preorder"))
		mode = GIT_TREEWALK_PRE;
	else if (id_mode == rb_intern("postorder"))
		mode = GIT_TREEWALK_POST;
	else
		rb_raise(rb_eTypeError,
				"Invalid iteration mode. Expected `:preorder` or `:postorder`");

	error = git_tree_walk(tree, mode, &rugged__treewalk_cb, (void *)&exception);

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    tree.path(path) -> entry
 *
 *  Retrieve and return a tree entry by its relative path.
 */
static VALUE rb_git_tree_path(VALUE self, VALUE rb_path)
{
	int error;
	git_tree *tree;
	git_tree_entry *entry;
	VALUE rb_entry;
	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);
	Check_Type(rb_path, T_STRING);

	error = git_tree_entry_bypath(&entry, tree, StringValueCStr(rb_path));
	rugged_exception_check(error);

	rb_entry = rb_git_treeentry_fromC(entry);
	git_tree_entry_free(entry);

	return rb_entry;
}

static VALUE rb_git_diff_tree_to_index(VALUE self, VALUE rb_repo, VALUE rb_self, VALUE rb_other, VALUE rb_options)
{
	git_tree *tree = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo = NULL;
	git_diff *diff = NULL;
	git_index *index;
	int error;

	Data_Get_Struct(rb_repo, git_repository, repo);
	Data_Get_Struct(rb_other, git_index, index);

	rugged_parse_diff_options(&opts, rb_options);

	if (RTEST(rb_self)) {
		TypedData_Get_Struct(rb_self, git_tree, &rugged_object_type, tree);
	}

	error = git_diff_tree_to_index(&diff, repo, tree, index, &opts);

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, rb_repo, diff);
}

struct nogvl_diff_args {
	git_repository * repo;
	git_tree * tree;
	git_tree * other_tree;
	git_diff_options * opts;
	int error;
};

static void * rb_git_diff_tree_to_tree_nogvl(void * _args)
{
	struct nogvl_diff_args * args;
	git_diff *diff = NULL;

	args = (struct nogvl_diff_args *)_args;
	args->error = git_diff_tree_to_tree(&diff, args->repo, args->tree, args->other_tree, args->opts);
	return diff;
}

static VALUE rb_git_diff_tree_to_tree(VALUE self, VALUE rb_repo, VALUE rb_tree, VALUE rb_other_tree, VALUE rb_options) {
	git_tree *tree = NULL;
	git_tree *other_tree = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo = NULL;
	git_diff *diff = NULL;
	struct nogvl_diff_args args;

	Data_Get_Struct(rb_repo, git_repository, repo);

	if(RTEST(rb_tree))
	    TypedData_Get_Struct(rb_tree, git_tree, &rugged_object_type, tree);

	if(RTEST(rb_other_tree))
	    TypedData_Get_Struct(rb_other_tree, git_tree, &rugged_object_type, other_tree);

	rugged_parse_diff_options(&opts, rb_options);

	args.repo = repo;
	args.tree = tree;
	args.other_tree = other_tree;
	args.opts = &opts;

	diff = rb_thread_call_without_gvl(rb_git_diff_tree_to_tree_nogvl, &args, RUBY_UBF_PROCESS, NULL);

	xfree(opts.pathspec.strings);
	rugged_exception_check(args.error);

	return rugged_diff_new(rb_cRuggedDiff, rb_repo, diff);
}

/*
 *  call-seq:
 *    tree.diff_workdir([options]) -> diff
 *
 *  Returns a diff between a tree and the current workdir.
 *
 *  The +tree+ object will be used as the "old file" side of the diff, while the
 *  content of the current workdir will be used for the "new file" side.
 *
 *  See Rugged::Tree#diff for a list of options that can be passed.
 */
static VALUE rb_git_tree_diff_workdir(int argc, VALUE *argv, VALUE self)
{
	git_tree *tree;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo;
	git_diff *diff;
	VALUE owner, rb_options;
	int error;

	rb_scan_args(argc, argv, "00:", &rb_options);
	rugged_parse_diff_options(&opts, rb_options);

	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);
	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	error = git_diff_tree_to_workdir(&diff, repo, tree, &opts);

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, owner, diff);
}

void rugged_parse_merge_options(git_merge_options *opts, VALUE rb_options)
{
	if (!NIL_P(rb_options)) {
		VALUE rb_value;
		Check_Type(rb_options, T_HASH);

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("rename_threshold"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->rename_threshold = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("target_limit"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->target_limit = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("favor"));
		if (!NIL_P(rb_value)) {
			ID id_favor;

			Check_Type(rb_value, T_SYMBOL);
			id_favor = SYM2ID(rb_value);

			if (id_favor == rb_intern("normal")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_NORMAL;
			} else if (id_favor == rb_intern("ours")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_OURS;
			} else if (id_favor == rb_intern("theirs")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_THEIRS;
			} else if (id_favor == rb_intern("union")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_UNION;
			} else {
				rb_raise(rb_eTypeError,
					"Invalid favor mode. Expected `:normal`, `:ours`, `:theirs` or `:union`");
			}
		}

		if (rb_hash_aref(rb_options, CSTR2SYM("renames")) == Qfalse) {
			opts->flags &= ~GIT_MERGE_FIND_RENAMES;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("fail_on_conflict")))) {
			opts->flags |= GIT_MERGE_FAIL_ON_CONFLICT;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("skip_reuc")))) {
			opts->flags |= GIT_MERGE_SKIP_REUC;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("no_recursive")))) {
			opts->flags |= GIT_MERGE_NO_RECURSIVE;
		}
	}
}

/*
 *  tree.merge(other_tree[, ancestor_tree[, options]]) -> Rugged::Index
 *  tree.merge(other_tree[, options]) -> Rugged::Index
 *
 *  Merges two trees and returns the a Rugged::Index object that reflects
 *  the result of the merge.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :renames ::
 *    If true, looking for renames will be enabled (`--find-renames`),
 *    set to false to disable (default true).
 *
 *  :rename_threshold ::
 *    An integer specifying the minimum similarity of a file to be
 *    seen as an eligible rename source (default 50).
 *
 *  :target_limit ::
 *    An integer specifying the maximum byte size of a file before a it will
 *    be treated as binary. The default value is 512MB.
 *
 *  :favor ::
 *    Specifies how and if conflicts are auto-resolved by favoring a specific
 *    file output. Can be one of `:normal`, `:ours`, `:theirs` or `:union`.
 *
 */
static VALUE rb_git_tree_merge(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_other_tree, rb_ancestor_tree, rb_options;
	VALUE rb_repo = rugged_owner(self);

	git_tree *tree, *other_tree, *ancestor_tree;
	git_repository *repo;
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	int error;

	if (rb_scan_args(argc, argv, "12", &rb_other_tree, &rb_ancestor_tree, &rb_options) == 2) {
		if (TYPE(rb_ancestor_tree) == T_HASH) {
			rb_options = rb_ancestor_tree;
			rb_ancestor_tree = Qnil;
		}
	}

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);
		rugged_parse_merge_options(&opts, rb_options);
	}

	if (!rb_obj_is_kind_of(rb_other_tree, rb_cRuggedTree))
		rb_raise(rb_eTypeError, "Expecting a Rugged::Tree instance");
	else if (!NIL_P(rb_ancestor_tree) && !rb_obj_is_kind_of(rb_ancestor_tree, rb_cRuggedTree))
		rb_raise(rb_eTypeError, "Expecting a Rugged::Tree instance");

	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);
	Data_Get_Struct(rb_repo, git_repository, repo);
	TypedData_Get_Struct(rb_other_tree, git_tree, &rugged_object_type, other_tree);

	if (!NIL_P(rb_ancestor_tree))
		TypedData_Get_Struct(rb_ancestor_tree, git_tree, &rugged_object_type, ancestor_tree);
	else
		ancestor_tree = NULL;

	error = git_merge_trees(&index, repo, ancestor_tree, tree, other_tree, &opts);
	if (error == GIT_EMERGECONFLICT)
		return Qnil;

	rugged_exception_check(error);

	return rugged_index_new(rb_cRuggedIndex, rb_repo, index);
}

static git_oid empty_tree = {{ 0x4b, 0x82, 0x5d, 0xc6, 0x42, 0xcb, 0x6e, 0xb9, 0xa0, 0x60,
			       0xe5, 0x4b, 0xf8, 0xd6, 0x92, 0x88, 0xfb, 0xee, 0x49, 0x04 }};

/*
 *  call-seq:
 *    Tree.empty(repo) -> tree
 *
 *  Look up the empty tree in the given repository +repo+. The empty
 *  tree's id is hard-coded to exist in a repository.
 *
 *  Returns a new instance of the empty tree.
 */
static VALUE rb_git_tree_empty(VALUE self, VALUE rb_repo)
{
	git_repository *repo;
	git_tree *tree;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	rugged_exception_check(git_tree_lookup(&tree, repo, &empty_tree));

	return rugged_object_new(rb_repo, (git_object *) tree);
}

/**
 * Parse the updates and convert them into libgit2 ones. They will be
 * heap-allocated and returned in 'out'. The strings will also be
 * heap-allocated and will be stored in 'strings'.
 */
static void parse_tree_updates(git_tree_update **out, int *nupdates_out, VALUE rb_updates)
{
	int i, nupdates;
	git_tree_update *updates;

	Check_Type(rb_updates, T_ARRAY);
	nupdates = RARRAY_LEN(rb_updates);
	updates = xcalloc(nupdates, sizeof(git_tree_update));

	for (i = 0; i < nupdates; i++) {
		VALUE rb_update = rb_ary_entry(rb_updates, i);
		VALUE rb_action, rb_oid, rb_filemode, rb_path;
		ID action;
		git_tree_update *update = &updates[i];

		if (!RB_TYPE_P(rb_update, T_HASH))
			goto on_error;

		rb_action   = rb_hash_aref(rb_update, CSTR2SYM("action"));
		rb_oid      = rb_hash_aref(rb_update, CSTR2SYM("oid"));
		rb_filemode = rb_hash_aref(rb_update, CSTR2SYM("filemode"));
		rb_path     = rb_hash_aref(rb_update, CSTR2SYM("path"));

		if (!SYMBOL_P(rb_action) || !RB_TYPE_P(rb_path, T_STRING))
			goto on_error;

		update->path = StringValueCStr(rb_path);

		action = SYM2ID(rb_action);

		if (action == rb_intern("upsert")) {
			if (!RB_TYPE_P(rb_oid, T_STRING) ||!RB_TYPE_P(rb_filemode, T_FIXNUM))
				goto on_error;

			update->action = GIT_TREE_UPDATE_UPSERT;
			update->filemode = NUM2INT(rb_filemode);

			if (git_oid_fromstr(&update->id, StringValueCStr(rb_oid)) < 0)
				goto on_error;
		} else if (action == rb_intern("remove")) {
			update->action = GIT_TREE_UPDATE_REMOVE;
		} else {
			goto on_error;
		}
	}

	*out = updates;
	*nupdates_out = nupdates;

	return;

on_error:
	xfree(updates);
	rb_raise(rb_eTypeError, "Invalid type for tree update object");
}

/*
 *  call-seq:
 *    tree.update(updates)
 *
 *  Create a new Rugged::Tree based on the curent one by applying the
 *  changes described in +updates+.
 *
 *  The updates are given as a list of +Hash+ containing:
 *
 *  :action ::
 *    +:upsert+ or +:remove+ to add/insert an entry, or to remove it resp.
 *
 *  :oid ::
 *    The +oid+ of the entry. This is ignored for removals.
 *
 *  :filemode ::
 *    The octal filemode for the entry. This is ignored for remvals.
 *
 *  :path ::
 *    The path of the entry. This may contain slashes and the
 *    intermediate trees will be created.
 *
 */
static VALUE rb_git_tree_update(VALUE self, VALUE rb_updates)
{
	git_repository *repo;
	git_tree *tree = NULL;
	git_tree_update *updates;
	int nupdates, error;
	git_oid id;

	TypedData_Get_Struct(self, git_tree, &rugged_object_type, tree);
	repo = git_tree_owner(tree);

	parse_tree_updates(&updates, &nupdates, rb_updates);

	error = git_tree_create_updated(&id, repo, tree, nupdates, updates);
	xfree(updates);

	rugged_exception_check(error);

	return rugged_create_oid(&id);
}

static void rb_git_treebuilder_free(git_treebuilder *bld)
{
	git_treebuilder_free(bld);
}

/*
 *  call-seq:
 *    Tree::Builder.new(repository, [tree])
 *
 *  Create a new Rugged::Tree::Builder instance to write a tree to
 *  the given +repository+.
 *
 *  If an optional +tree+ is given, the returned Tree::Builder will be
 *  initialized with the entry of +tree+. Otherwise, the Tree::Builder
 *  will be empty and has to be filled manually.
 */
static VALUE rb_git_treebuilder_new(int argc, VALUE *argv, VALUE klass)
{
	git_treebuilder *builder;
	git_repository *repo;
	git_tree *tree = NULL;
	VALUE rb_object, rb_builder, rb_repo;
	int error;

	if (rb_scan_args(argc, argv, "11", &rb_repo, &rb_object) == 2) {
		if (!rb_obj_is_kind_of(rb_object, rb_cRuggedTree))
			rb_raise(rb_eTypeError, "A Rugged::Tree instance is required");

		TypedData_Get_Struct(rb_object, git_tree, &rugged_object_type, tree);
	}

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_treebuilder_new(&builder, repo, tree);
	rugged_exception_check(error);

	rb_builder = Data_Wrap_Struct(klass, NULL, &rb_git_treebuilder_free, builder);
	rugged_set_owner(rb_builder, rb_repo);

	return rb_builder;
}

/*
 *  call-seq:
 *    builder.clear -> nil
 *
 *  Clear all entries in +builder+.
 */
static VALUE rb_git_treebuilder_clear(VALUE self)
{
	git_treebuilder *builder;
	Data_Get_Struct(self, git_treebuilder, builder);
	git_treebuilder_clear(builder);
	return Qnil;
}

/*
 *  call-seq:
 *    builder[path] -> entry
 *
 *  Return an entry from +builder+ based on its relative path.
 */
static VALUE rb_git_treebuilder_get(VALUE self, VALUE path)
{
	git_treebuilder *builder;
	Data_Get_Struct(self, git_treebuilder, builder);

	Check_Type(path, T_STRING);

	return rb_git_treeentry_fromC(git_treebuilder_get(builder, StringValueCStr(path)));
}

/*
 *  call-seq:
 *    builder << entry      -> nil
 *    builder.insert(entry) -> nil
 *
 *  Inser a new entry into +builder+.
 */
static VALUE rb_git_treebuilder_insert(VALUE self, VALUE rb_entry)
{
	git_treebuilder *builder;
	VALUE rb_path, rb_oid, rb_attr;
	git_oid oid;
	int error;

	Data_Get_Struct(self, git_treebuilder, builder);
	Check_Type(rb_entry, T_HASH);

	rb_path = rb_hash_aref(rb_entry, CSTR2SYM("name"));
	Check_Type(rb_path, T_STRING);

	rb_oid = rb_hash_aref(rb_entry, CSTR2SYM("oid"));
	Check_Type(rb_oid, T_STRING);
	rugged_exception_check(git_oid_fromstr(&oid, StringValueCStr(rb_oid)));

	rb_attr = rb_hash_aref(rb_entry, CSTR2SYM("filemode"));
	Check_Type(rb_attr, T_FIXNUM);

	error = git_treebuilder_insert(NULL,
		builder,
		StringValueCStr(rb_path),
		&oid,
		FIX2INT(rb_attr));

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    builder.remove(path) -> true or false
 *
 *  Remove an entry from +builder+ by its relative +path+.
 *
 *  Returns +true+ if the entry was successfully removed,
 *  or +false+ if the entry was not found.
 */
static VALUE rb_git_treebuilder_remove(VALUE self, VALUE path)
{
	git_treebuilder *builder;
	int error;

	Data_Get_Struct(self, git_treebuilder, builder);
	Check_Type(path, T_STRING);

	error = git_treebuilder_remove(builder, StringValueCStr(path));
	if (error == GIT_ENOTFOUND) {
		return Qfalse;
	} else if (error == GIT_ERROR && giterr_last()->klass == GITERR_TREE) {
		return Qfalse;
	}

	rugged_exception_check(error);
	return Qtrue;
}

/*
 *  call-seq:
 *    builder.write -> oid
 *
 *  Write +builder+'s content as a tree to the repository
 *  that owns the builder and return the +oid+ for the
 *  newly created tree.
 */
static VALUE rb_git_treebuilder_write(VALUE self)
{
	git_treebuilder *builder;
	git_oid written_id;
	int error;

	Data_Get_Struct(self, git_treebuilder, builder);

	error = git_treebuilder_write(&written_id, builder);
	rugged_exception_check(error);

	return rugged_create_oid(&written_id);
}

static int treebuilder_cb(const git_tree_entry *entry, void *opaque)
{
	VALUE proc = (VALUE)opaque;
	VALUE ret = rb_funcall(proc, rb_intern("call"), 1, rb_git_treeentry_fromC(entry));
	return rugged_parse_bool(ret);
}

/*
 *  call-seq:
 *    builder.reject! { |entry| block } -> nil
 *
 *  Deletes every tree +entry+ from +builder+ for which
 *  the given +block+ evaluates to true.
 */
static VALUE rb_git_treebuilder_filter(VALUE self)
{
	git_treebuilder *builder;

	rb_need_block();
	Data_Get_Struct(self, git_treebuilder, builder);

	git_treebuilder_filter(builder, &treebuilder_cb, (void *)rb_block_proc());
	return Qnil;
}

void Init_rugged_tree(void)
{
	/*
	 * Tree
	 */
	rb_cRuggedTree = rb_define_class_under(rb_mRugged, "Tree", rb_cRuggedObject);
	rb_undef_alloc_func(rb_cRuggedTree);

	rb_define_method(rb_cRuggedTree, "count", rb_git_tree_entrycount, 0);
	rb_define_method(rb_cRuggedTree, "count_recursive", rb_git_tree_entrycount_recursive, -1);
	rb_define_method(rb_cRuggedTree, "length", rb_git_tree_entrycount, 0);
	rb_define_method(rb_cRuggedTree, "get_entry", rb_git_tree_get_entry, 1);
	rb_define_method(rb_cRuggedTree, "get_entry_by_oid", rb_git_tree_get_entry_by_oid, 1);
	rb_define_method(rb_cRuggedTree, "path", rb_git_tree_path, 1);
	rb_define_method(rb_cRuggedTree, "diff_workdir", rb_git_tree_diff_workdir, -1);
	rb_define_method(rb_cRuggedTree, "[]", rb_git_tree_get_entry, 1);
	rb_define_method(rb_cRuggedTree, "each", rb_git_tree_each, 0);
	rb_define_method(rb_cRuggedTree, "walk", rb_git_tree_walk, 1);
	rb_define_method(rb_cRuggedTree, "merge", rb_git_tree_merge, -1);
	rb_define_method(rb_cRuggedTree, "update", rb_git_tree_update, 1);
	rb_define_singleton_method(rb_cRuggedTree, "empty", rb_git_tree_empty, 1);

	rb_define_private_method(rb_singleton_class(rb_cRuggedTree), "diff_tree_to_index", rb_git_diff_tree_to_index, 4);
	rb_define_private_method(rb_singleton_class(rb_cRuggedTree), "diff_tree_to_tree", rb_git_diff_tree_to_tree, 4);

	rb_cRuggedTreeBuilder = rb_define_class_under(rb_cRuggedTree, "Builder", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedTreeBuilder);

	rb_define_singleton_method(rb_cRuggedTreeBuilder, "new", rb_git_treebuilder_new, -1);
	rb_define_method(rb_cRuggedTreeBuilder, "clear", rb_git_treebuilder_clear, 0);
	rb_define_method(rb_cRuggedTreeBuilder, "[]", rb_git_treebuilder_get, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "insert", rb_git_treebuilder_insert, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "<<", rb_git_treebuilder_insert, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "remove", rb_git_treebuilder_remove, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "write", rb_git_treebuilder_write, 0);
	rb_define_method(rb_cRuggedTreeBuilder, "reject!", rb_git_treebuilder_filter, 0);
}
