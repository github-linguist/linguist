/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_eRuggedError;
extern VALUE rb_cRuggedRemote;
VALUE rb_cRuggedRemoteCollection;

/*
 *  call-seq:
 *    RemoteCollection.new(repo) -> remotes
 *
 *  Creates and returns a new collection of remotes for the given +repo+.
 */
static VALUE rb_git_remote_collection_initialize(VALUE self, VALUE repo)
{
	rugged_set_owner(self, repo);
	return self;
}

/*
 *  call-seq:
 *    remotes.create_anonymous(url) -> remote
 *
 *  Return a new remote with +url+ in +repository+ , the remote is not persisted:
 *  - +url+: a valid remote url
 *
 *  Returns a new Rugged::Remote object.
 *
 *    @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git') #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_collection_create_anonymous(VALUE self, VALUE rb_url)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	VALUE rb_repo = rugged_owner(self);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_url, T_STRING);

	error = git_remote_create_anonymous(
			&remote,
			repo,
			StringValueCStr(rb_url));

	rugged_exception_check(error);

	return rugged_remote_new(rb_repo, remote);
}

/*
 *  call-seq:
 *     remotes.create(name, url) -> remote
 *
 *  Add a new remote with +name+ and +url+ to +repository+
 *  - +url+: a valid remote url
 *  - +name+: a valid remote name
 *
 *  Returns a new Rugged::Remote object.
 *
 *    @repo.remotes.create('origin', 'git://github.com/libgit2/rugged.git') #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_collection_create(VALUE self, VALUE rb_name, VALUE rb_url)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	VALUE rb_repo = rugged_owner(self);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_name, T_STRING);
	Check_Type(rb_url, T_STRING);

	error = git_remote_create(
			&remote,
			repo,
			StringValueCStr(rb_name),
			StringValueCStr(rb_url));

	rugged_exception_check(error);

	return rugged_remote_new(rb_repo, remote);
}

/*
 *  call-seq:
 *    remotes[name] -> remote or nil
 *
 *  Lookup a remote in the collection with the given +name+.
 *
 *  Returns a new Rugged::Remote object or +nil+ if the
 *  remote doesn't exist.
 *
 *    @repo.remotes["origin"] #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_collection_aref(VALUE self, VALUE rb_name)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	VALUE rb_repo = rugged_owner(self);
	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_name, T_STRING);

	error = git_remote_lookup(&remote, repo, StringValueCStr(rb_name));

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_remote_new(rb_repo, remote);
}

static VALUE rb_git_remote_collection__each(VALUE self, int only_names)
{
	git_repository *repo;
	git_strarray remotes;
	size_t i;
	int error = 0;
	int exception = 0;

	VALUE rb_repo;

	RETURN_ENUMERATOR(self, 0, 0);

	rb_repo = rugged_owner(self);
	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_list(&remotes, repo);
	rugged_exception_check(error);

	if (only_names) {
		for (i = 0; !exception && i < remotes.count; ++i) {
			rb_protect(rb_yield, rb_str_new_utf8(remotes.strings[i]), &exception);
		}
	} else {
		for (i = 0; !exception && !error && i < remotes.count; ++i) {
			git_remote *remote;

			if (!(error = git_remote_lookup(&remote, repo, remotes.strings[i])))
				rb_protect(rb_yield, rugged_remote_new(rb_repo, remote), &exception);
		}
	}

	git_strarray_free(&remotes);

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return Qnil;
}


/*
 *  call-seq:
 *    remotes.each { |remote| } -> nil
 *    remotes.each -> enumerator
 *
 *  Iterate through all the remotes in the collection's +repository+.
 *
 *  The given block will be called once with a Rugged::Remote
 *  instance for each remote.
 *
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_remote_collection_each(VALUE self)
{
	return rb_git_remote_collection__each(self, 0);
}

/*
 *  call-seq:
 *    remotes.each_name { |str| } -> nil
 *    remotes.each_name -> enumerator
 *
 *  Iterate through all the remote names in the collection's +repository+.
 *
 *  The given block will be called once with the name of each remote.
 *
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_remote_collection_each_name(VALUE self)
{
	return rb_git_remote_collection__each(self, 1);
}

/*
 *  call-seq:
 *    remotes.rename(remote, new_name) { |str| }  -> remote
 *    remotes.rename(name, new_name) { |str| } -> remote
 *
 *  Renames a remote.
 *
 *  All remote-tracking branches and configuration settings
 *  for the remote are updated.
 *
 *  Non-default refspecs cannot be renamed automatically and will be
 *  yielded to the given block.
 *
 *  Anonymous, in-memory remotes created through
 *  +ReferenceCollection#create_anonymous+ can not be given a name through
 *  this method.
 *
 *  Returns a new Rugged::Remote object with the new name.
 */
static VALUE rb_git_remote_collection_rename(VALUE self, VALUE rb_name_or_remote, VALUE rb_new_name)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;
	size_t i;
	int error, exception;
	git_strarray problems;

	if (!rb_block_given_p())
		rb_raise(rb_eArgError, "Rugged::RemoteCollection#rename must be called with a block");

	Check_Type(rb_new_name, T_STRING);

	if (rb_obj_is_kind_of(rb_name_or_remote, rb_cRuggedRemote))
		rb_name_or_remote = rb_funcall(rb_name_or_remote, rb_intern("name"), 0);

	if (TYPE(rb_name_or_remote) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Remote instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_rename(&problems, repo, StringValueCStr(rb_name_or_remote), StringValueCStr(rb_new_name));
	rugged_exception_check(error);

	for (i = exception = 0; !exception && i < problems.count; ++i) {
		rb_protect(rb_yield, rb_str_new_utf8(problems.strings[i]), &exception);
	}

	git_strarray_free(&problems);

	if (exception)
		rb_jump_tag(exception);

	return rb_git_remote_collection_aref(self, rb_new_name);
}

/*
 *  call-seq:
 *    remotes.delete(remote) -> nil
 *    remotes.delete(name) -> nil
 *
 *  Delete the specified remote.
 *
 *    repo.remotes.delete("origin")
 *    # Remote no longer exists in the configuration.
 */
static VALUE rb_git_remote_collection_delete(VALUE self, VALUE rb_name_or_remote)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;

	if (rb_obj_is_kind_of(rb_name_or_remote, rb_cRuggedRemote))
		rb_name_or_remote = rb_funcall(rb_name_or_remote, rb_intern("name"), 0);

	if (TYPE(rb_name_or_remote) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Remote instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	rugged_exception_check(
		git_remote_delete(repo, StringValueCStr(rb_name_or_remote))
	);

	return Qnil;
}

/*
 *  call-seq:
 *    remotes.set_url(remote, url) -> nil
 *    remotes.set_url(name, url) -> nil
 *
 *  Sets the remote's url in the configuration.
 *  Rugged::Remote objects already in memory will not be affected.
 *
 *    repo.remotes.set_url("origin", 'git://github.com/libgit2/rugged.git')
 */
static VALUE rb_git_remote_collection_set_url(VALUE self, VALUE rb_name_or_remote, VALUE rb_url)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;

	if (rb_obj_is_kind_of(rb_name_or_remote, rb_cRuggedRemote))
		rb_name_or_remote = rb_funcall(rb_name_or_remote, rb_intern("name"), 0);

	if (TYPE(rb_name_or_remote) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Remote instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_url, T_STRING);

	rugged_exception_check(
		git_remote_set_url(repo, StringValueCStr(rb_name_or_remote), StringValueCStr(rb_url))
	);

	return Qnil;
}

/*
 *  call-seq:
 *    remotes.set_push_url(remote, url) -> nil
 *    remotes.set_push_url(name, url) -> nil
 *
 *  Sets the remote's url for pushing in the configuration.
 *  Rugged::Remote objects already in memory will not be affected.
 *
 *    repo.remotes.set_push_url("origin", 'git://github.com/libgit2/rugged.git')
 */
static VALUE rb_git_remote_collection_set_push_url(VALUE self, VALUE rb_name_or_remote, VALUE rb_url)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;

	if (rb_obj_is_kind_of(rb_name_or_remote, rb_cRuggedRemote))
		rb_name_or_remote = rb_funcall(rb_name_or_remote, rb_intern("name"), 0);

	if (TYPE(rb_name_or_remote) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Remote instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_url, T_STRING);

	rugged_exception_check(
		git_remote_set_pushurl(repo, StringValueCStr(rb_name_or_remote), StringValueCStr(rb_url))
	);

	return Qnil;
}

static VALUE rb_git_remote_collection_add_refspec(VALUE self, VALUE rb_name_or_remote, VALUE rb_refspec, git_direction direction)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;
	int error = 0;

	if (rb_obj_is_kind_of(rb_name_or_remote, rb_cRuggedRemote))
		rb_name_or_remote = rb_funcall(rb_name_or_remote, rb_intern("name"), 0);

	if (TYPE(rb_name_or_remote) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Remote instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_refspec, T_STRING);

	if (direction == GIT_DIRECTION_FETCH)
		error = git_remote_add_fetch(repo, StringValueCStr(rb_name_or_remote), StringValueCStr(rb_refspec));
	else
		error = git_remote_add_push(repo, StringValueCStr(rb_name_or_remote), StringValueCStr(rb_refspec));

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    remotes.add_fetch_refspec(remote, refspec) -> nil
 *    remotes.add_fetch_refspec(name, refspec) -> nil
 *
 *  Add a fetch refspec to the remote.
 */
static VALUE rb_git_remote_collection_add_fetch_refspec(VALUE self, VALUE rb_name_or_remote, VALUE rb_refspec)
{
	return rb_git_remote_collection_add_refspec(self, rb_name_or_remote, rb_refspec, GIT_DIRECTION_FETCH);
}

/*
 *  call-seq:
 *    remotes.add_push_refspec(remote, refspec) -> nil
 *    remotes.add_push_refspec(name, refspec) -> nil
 *
 *  Add a push refspec to the remote.
 */
static VALUE rb_git_remote_collection_add_push_refspec(VALUE self, VALUE rb_name_or_remote, VALUE rb_refspec)
{
	return rb_git_remote_collection_add_refspec(self, rb_name_or_remote, rb_refspec, GIT_DIRECTION_PUSH);
}

void Init_rugged_remote_collection(void)
{
	rb_cRuggedRemoteCollection = rb_define_class_under(rb_mRugged, "RemoteCollection", rb_cObject);
	rb_include_module(rb_cRuggedRemoteCollection, rb_mEnumerable);

	rb_define_method(rb_cRuggedRemoteCollection, "initialize",        rb_git_remote_collection_initialize, 1);

	rb_define_method(rb_cRuggedRemoteCollection, "[]",                rb_git_remote_collection_aref, 1);

	rb_define_method(rb_cRuggedRemoteCollection, "create",            rb_git_remote_collection_create, 2);
	rb_define_method(rb_cRuggedRemoteCollection, "create_anonymous",  rb_git_remote_collection_create_anonymous, 1);

	rb_define_method(rb_cRuggedRemoteCollection, "each",              rb_git_remote_collection_each, 0);
	rb_define_method(rb_cRuggedRemoteCollection, "each_name",         rb_git_remote_collection_each_name, 0);

	rb_define_method(rb_cRuggedRemoteCollection, "set_url",           rb_git_remote_collection_set_url, 2);
	rb_define_method(rb_cRuggedRemoteCollection, "set_push_url",      rb_git_remote_collection_set_push_url, 2);

	rb_define_method(rb_cRuggedRemoteCollection, "add_push_refspec",  rb_git_remote_collection_add_push_refspec, 2);
	rb_define_method(rb_cRuggedRemoteCollection, "add_fetch_refspec", rb_git_remote_collection_add_fetch_refspec, 2);

	rb_define_method(rb_cRuggedRemoteCollection, "rename",            rb_git_remote_collection_rename, 2);
	rb_define_method(rb_cRuggedRemoteCollection, "delete",            rb_git_remote_collection_delete, 1);
}
