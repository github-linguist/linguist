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
VALUE rb_cRuggedRemote;

static int progress_cb(const char *str, int len, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(2);

	if (NIL_P(payload->progress))
		return 0;

	rb_ary_push(args, payload->progress);
	rb_ary_push(args, rb_str_new(str, len));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

static int transfer_progress_cb(const git_transfer_progress *stats, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(5);

	if (NIL_P(payload->transfer_progress))
		return 0;

	rb_ary_push(args, payload->transfer_progress);
	rb_ary_push(args, UINT2NUM(stats->total_objects));
	rb_ary_push(args, UINT2NUM(stats->indexed_objects));
	rb_ary_push(args, UINT2NUM(stats->received_objects));
	rb_ary_push(args, UINT2NUM(stats->local_objects));
	rb_ary_push(args, UINT2NUM(stats->total_deltas));
	rb_ary_push(args, UINT2NUM(stats->indexed_deltas));
	rb_ary_push(args, INT2FIX(stats->received_bytes));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

static int push_update_reference_cb(const char *refname, const char *status, void *data) {
	struct rugged_remote_cb_payload *payload = data;

	if (status != NULL)
		rb_hash_aset(payload->result, rb_str_new_utf8(refname), rb_str_new_utf8(status));

	return GIT_OK;
}

static int update_tips_cb(const char *refname, const git_oid *src, const git_oid *dest, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(4);

	if (NIL_P(payload->update_tips))
		return 0;

	rb_ary_push(args, payload->update_tips);
	rb_ary_push(args, rb_str_new_utf8(refname));
	rb_ary_push(args, git_oid_iszero(src) ? Qnil : rugged_create_oid(src));
	rb_ary_push(args, git_oid_iszero(dest) ? Qnil : rugged_create_oid(dest));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

static int certificate_check_cb(git_cert *cert, int valid, const char *host, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(3);
	VALUE ret;

	if (NIL_P(payload->certificate_check))
		return valid ? 0 : GIT_ECERTIFICATE;

	rb_ary_push(args, payload->certificate_check);
	rb_ary_push(args, valid ? Qtrue : Qfalse);
	rb_ary_push(args, rb_str_new_utf8(host));

	ret = rb_protect(rugged__block_yield_splat, args, &payload->exception);

	if (payload->exception)
		return GIT_ERROR;

	return rugged_parse_bool(ret) ? GIT_OK : GIT_ECERTIFICATE;
}

struct extract_cred_args
{
	VALUE rb_callback;
	git_cred **cred;
	const char *url;
	const char *username_from_url;
	unsigned int allowed_types;
};

static VALUE allowed_types_to_rb_ary(int allowed_types) {
	VALUE rb_allowed_types = rb_ary_new();

	if (allowed_types & GIT_CREDTYPE_USERPASS_PLAINTEXT)
		rb_ary_push(rb_allowed_types, CSTR2SYM("plaintext"));

	if (allowed_types & GIT_CREDTYPE_SSH_KEY)
		rb_ary_push(rb_allowed_types, CSTR2SYM("ssh_key"));

	if (allowed_types & GIT_CREDTYPE_DEFAULT)
		rb_ary_push(rb_allowed_types, CSTR2SYM("default"));

	return rb_allowed_types;
}

static VALUE extract_cred(VALUE data) {
	struct extract_cred_args *args = (struct extract_cred_args*)data;
	VALUE rb_url, rb_username_from_url, rb_cred;

	rb_url = args->url ? rb_str_new2(args->url) : Qnil;
	rb_username_from_url = args->username_from_url ? rb_str_new2(args->username_from_url) : Qnil;

	rb_cred = rb_funcall(args->rb_callback, rb_intern("call"), 3,
		rb_url, rb_username_from_url, allowed_types_to_rb_ary(args->allowed_types));

	rugged_cred_extract(args->cred, args->allowed_types, rb_cred);

	return Qnil;
}

static int credentials_cb(
	git_cred **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	struct extract_cred_args args = {
		payload->credentials, cred, url, username_from_url, allowed_types
	};

	if (NIL_P(payload->credentials))
		return GIT_PASSTHROUGH;

	rb_protect(extract_cred, (VALUE)&args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

void rugged_remote_init_callbacks_and_payload_from_options(
	VALUE rb_options,
	git_remote_callbacks *callbacks,
	struct rugged_remote_cb_payload *payload)
{
	callbacks->payload = payload;
	callbacks->push_update_reference = push_update_reference_cb;

	if (!NIL_P(rb_options)) {
		payload->progress = rb_hash_aref(rb_options, CSTR2SYM("progress"));
		if (!NIL_P(payload->progress)) {
			CALLABLE_OR_RAISE(payload->progress, "progress");
			callbacks->sideband_progress = progress_cb;
		}

		payload->credentials = rb_hash_aref(rb_options, CSTR2SYM("credentials"));
		if (!NIL_P(payload->credentials)) {
			CALLABLE_OR_RAISE(payload->credentials, "credentials");
			callbacks->credentials = credentials_cb;
		}

		payload->certificate_check = rb_hash_aref(rb_options, CSTR2SYM("certificate_check"));
		if (!NIL_P(payload->certificate_check)) {
			CALLABLE_OR_RAISE(payload->certificate_check, "certificate_check");
			callbacks->certificate_check = certificate_check_cb;
		}

		payload->transfer_progress = rb_hash_aref(rb_options, CSTR2SYM("transfer_progress"));
		if (!NIL_P(payload->transfer_progress)) {
			CALLABLE_OR_RAISE(payload->transfer_progress, "transfer_progress");
			callbacks->transfer_progress = transfer_progress_cb;
		}

		payload->update_tips = rb_hash_aref(rb_options, CSTR2SYM("update_tips"));
		if (!NIL_P(payload->update_tips)) {
			CALLABLE_OR_RAISE(payload->update_tips, "update_tips");
			callbacks->update_tips = update_tips_cb;
		}
	}
}

void rugged_remote_init_custom_headers(VALUE rb_options, git_strarray *custom_headers)
{
	if (!NIL_P(rb_options))
	{
		VALUE rb_headers = rb_hash_aref(rb_options, CSTR2SYM("headers"));
		rugged_rb_ary_to_strarray(rb_headers, custom_headers);
	}
}

void rugged_remote_init_proxy_options(VALUE rb_options, git_proxy_options *proxy_options)
{
	if (NIL_P(rb_options)) return;

	VALUE val = rb_hash_aref(rb_options, CSTR2SYM("proxy_url"));
	if (!NIL_P(val)) {
		Check_Type(val, T_STRING);
		proxy_options->type = GIT_PROXY_SPECIFIED;
		proxy_options->url = StringValueCStr(val);
	}
}

static void init_pb_parallelism(VALUE rb_options, git_push_options *opts)
{
	if (NIL_P(rb_options)) return;

	VALUE val = rb_hash_aref(rb_options, CSTR2SYM("pb_parallelism"));
	if (!NIL_P(val)) {
		Check_Type(val, T_FIXNUM);
		opts->pb_parallelism = FIX2UINT(val);
	}
}

static int parse_prune_type(VALUE rb_prune_type)
{
	if (rb_prune_type == Qtrue) {
		return GIT_FETCH_PRUNE;
	} else if (rb_prune_type == Qfalse) {
		return GIT_FETCH_NO_PRUNE;
	} else if (rb_prune_type == Qnil) {
		return GIT_FETCH_PRUNE_UNSPECIFIED;
	} else {
		rb_raise(rb_eTypeError, "wrong argument type for :prune (expected true, false or nil)");
	}
}

static void rb_git_remote__free(git_remote *remote)
{
	git_remote_free(remote);
}

VALUE rugged_remote_new(VALUE owner, git_remote *remote)
{
	VALUE rb_remote;

	rb_remote = Data_Wrap_Struct(rb_cRuggedRemote, NULL, &rb_git_remote__free, remote);
	rugged_set_owner(rb_remote, owner);
	return rb_remote;
}

static VALUE rugged_rhead_new(const git_remote_head *head)
{
	VALUE rb_head = rb_hash_new();

	rb_hash_aset(rb_head, CSTR2SYM("local?"), head->local ? Qtrue : Qfalse);
	rb_hash_aset(rb_head, CSTR2SYM("oid"), rugged_create_oid(&head->oid));
	rb_hash_aset(rb_head, CSTR2SYM("loid"),
			git_oid_iszero(&head->loid) ? Qnil : rugged_create_oid(&head->loid));
	rb_hash_aset(rb_head, CSTR2SYM("name"), rb_str_new_utf8(head->name));

	return rb_head;
}

/*
 *  call-seq:
 *    remote.ls(options = {}) -> an_enumerator
 *    remote.ls(options = {}) { |remote_head_hash| block }
 *
 *  Connects +remote+ to list all references available along with their
 *  associated commit ids.
 *
 *  The given block is called once for each remote head with a Hash containing the
 *  following keys:
 *
 *  :local? ::
 *    +true+ if the remote head is available locally, +false+ otherwise.
 *
 *  :oid ::
 *    The id of the object the remote head is currently pointing to.
 *
 *  :loid ::
 *    The id of the object the local copy of the remote head is currently
 *    pointing to. Set to +nil+ if there is no local copy of the remote head.
 *
 *  :name ::
 *    The fully qualified reference name of the remote head.
 *
 *  If no block is given, an enumerator will be returned.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the ls operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :headers ::
 *    Extra HTTP headers to include with the request (only applies to http:// or https:// remotes)
 *
 *  :proxy_url ::
 *    The url of an http proxy to use to access the remote repository.
 */
static VALUE rb_git_remote_ls(int argc, VALUE *argv, VALUE self)
{
	git_remote *remote;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	git_proxy_options proxy_options = GIT_PROXY_OPTIONS_INIT;
	git_strarray custom_headers = {0};
	const git_remote_head **heads;

	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, 0 };

	VALUE rb_options;

	int error;
	size_t heads_len, i;

	RETURN_ENUMERATOR(self, argc, argv);
	Data_Get_Struct(self, git_remote, remote);
	rb_scan_args(argc, argv, ":", &rb_options);

	rugged_remote_init_callbacks_and_payload_from_options(rb_options, &callbacks, &payload);
	rugged_remote_init_custom_headers(rb_options, &custom_headers);
	rugged_remote_init_proxy_options(rb_options, &proxy_options);

	if ((error = git_remote_connect(remote, GIT_DIRECTION_FETCH, &callbacks, &proxy_options, &custom_headers)) ||
	    (error = git_remote_ls(&heads, &heads_len, remote)))
		goto cleanup;

	for (i = 0; i < heads_len && !payload.exception; i++)
		rb_protect(rb_yield, rugged_rhead_new(heads[i]), &payload.exception);

	cleanup:

	git_remote_disconnect(remote);
	xfree(custom_headers.strings);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    remote.name() -> string
 *
 *	Returns the remote's name.
 *
 *	  remote.name #=> "origin"
 */
static VALUE rb_git_remote_name(VALUE self)
{
	git_remote *remote;
	const char * name;
	Data_Get_Struct(self, git_remote, remote);

	name = git_remote_name(remote);

	return name ? rb_str_new_utf8(name) : Qnil;
}

/*
 *  call-seq:
 *    remote.url() -> string
 *
 *  Returns the remote's url
 *
 *    remote.url #=> "git://github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_url(VALUE self)
{
	git_remote *remote;
	Data_Get_Struct(self, git_remote, remote);

	return rb_str_new_utf8(git_remote_url(remote));
}

/*
 *  call-seq:
 *    remote.push_url() -> string or nil
 *
 *  Returns the remote's url for pushing or nil if no special url for
 *  pushing is set.
 *
 *    remote.push_url #=> "git://github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_push_url(VALUE self)
{
	git_remote *remote;
	const char * push_url;

	Data_Get_Struct(self, git_remote, remote);

	push_url = git_remote_pushurl(remote);
	return push_url ? rb_str_new_utf8(push_url) : Qnil;
}

/*
 *  call-seq:
 *    remote.push_url = url -> url
 *
 *  Sets the remote's url for pushing without persisting it in the config.
 *  Existing connections will not be updated.
 *
 *    remote.push_url = 'git@github.com/libgit2/rugged.git' #=> "git@github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_set_push_url(VALUE self, VALUE rb_url)
{
	VALUE rb_repo = rugged_owner(self);
	git_remote *remote;
	git_repository *repo;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_url, T_STRING);
	Data_Get_Struct(self, git_remote, remote);

	rugged_exception_check(
		git_remote_set_pushurl(repo, git_remote_name(remote), StringValueCStr(rb_url))
	);

	return rb_url;
}

static VALUE rb_git_remote_refspecs(VALUE self, git_direction direction)
{
	git_remote *remote;
	int error = 0;
	git_strarray refspecs;
	VALUE rb_refspec_array;

	Data_Get_Struct(self, git_remote, remote);

	if (direction == GIT_DIRECTION_FETCH)
		error = git_remote_get_fetch_refspecs(&refspecs, remote);
	else
		error = git_remote_get_push_refspecs(&refspecs, remote);

	rugged_exception_check(error);

	rb_refspec_array = rugged_strarray_to_rb_ary(&refspecs);
	git_strarray_free(&refspecs);
	return rb_refspec_array;
}

/*
 *  call-seq:
 *  remote.fetch_refspecs -> array
 *
 *  Get the remote's list of fetch refspecs as +array+.
 */
static VALUE rb_git_remote_fetch_refspecs(VALUE self)
{
	return rb_git_remote_refspecs(self, GIT_DIRECTION_FETCH);
}

/*
 *  call-seq:
 *  remote.push_refspecs -> array
 *
 *  Get the remote's list of push refspecs as +array+.
 */
static VALUE rb_git_remote_push_refspecs(VALUE self)
{
	return rb_git_remote_refspecs(self, GIT_DIRECTION_PUSH);
}

/*
 *  call-seq:
 *    remote.check_connection(direction, options = {}) -> boolean
 *
 *  Try to connect to the +remote+. Useful to simulate
 *  <tt>git fetch --dry-run</tt> and <tt>git push --dry-run</tt>.
 *
 *  Returns +true+ if connection is successful, +false+ otherwise.
 *
 *  +direction+ must be either +:fetch+ or +:push+.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  +credentials+ ::
 *    The credentials to use for the connection. Can be either an instance of
 *    one of the Rugged::Credentials types, or a proc returning one of the
 *    former.
 *    The proc will be called with the +url+, the +username+ from the url (if
 *    applicable) and a list of applicable credential types.
 *
 *  :headers ::
 *    Extra HTTP headers to include with the request (only applies to http:// or https:// remotes)
 *
 *  :proxy_url ::
 *    The url of an http proxy to use to access the remote repository.
 *
 *  Example:
 *
 *    remote = repo.remotes["origin"]
 *    success = remote.check_connection(:fetch)
 *    raise Error("Unable to pull without credentials") unless success
 */
static VALUE rb_git_remote_check_connection(int argc, VALUE *argv, VALUE self)
{
	git_remote *remote;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	git_proxy_options proxy_options = GIT_PROXY_OPTIONS_INIT;
	git_strarray custom_headers = {0};
	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, 0 };
	VALUE rb_direction, rb_options;
	ID id_direction;
	int error, direction;

	Data_Get_Struct(self, git_remote, remote);
	rb_scan_args(argc, argv, "01:", &rb_direction, &rb_options);

	Check_Type(rb_direction, T_SYMBOL);
	id_direction = SYM2ID(rb_direction);
	if (id_direction == rb_intern("fetch"))
		direction = GIT_DIRECTION_FETCH;
	else if (id_direction == rb_intern("push"))
		direction = GIT_DIRECTION_PUSH;
	else
		rb_raise(rb_eTypeError, "Invalid direction. Expected :fetch or :push");

	rugged_remote_init_callbacks_and_payload_from_options(rb_options, &callbacks, &payload);
	rugged_remote_init_custom_headers(rb_options, &custom_headers);
	rugged_remote_init_proxy_options(rb_options, &proxy_options);

	error = git_remote_connect(remote, direction, &callbacks, &proxy_options, &custom_headers);
	git_remote_disconnect(remote);

	xfree(custom_headers.strings);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	return error ? Qfalse : Qtrue;
}

/*
 *  call-seq:
 *    remote.fetch(refspecs = nil, options = {}) -> hash
 *
 *  Downloads new data from the remote for the given +refspecs+ and updates tips.
 *
 *  You can optionally pass in a single or multiple alternative +refspecs+ to use instead of the fetch
 *  refspecs already configured for +remote+.
 *
 *  Returns a hash containing statistics for the fetch operation.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the fetch operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :headers ::
 *    Extra HTTP headers to include with the request (only applies to http:// or https:// remotes)
 *
 *  :progress ::
 *    A callback that will be executed with the textual progress received from the remote.
 *    This is the text send over the progress side-band (ie. the "counting objects" output).
 *
 *  :transfer_progress ::
 *    A callback that will be executed to report clone progress information. It will be passed
 *    the amount of +total_objects+, +indexed_objects+, +received_objects+, +local_objects+,
 *    +total_deltas+, +indexed_deltas+ and +received_bytes+.
 *
 *  :update_tips ::
 *    A callback that will be executed each time a reference is updated locally. It will be
 *    passed the +refname+, +old_oid+ and +new_oid+.
 *
 *  :certificate_check ::
 *    A callback that will be executed each time we validate a certificate using https. It
 *    will be passed the +valid+, +host_name+ and the callback should return a true/false to
 *    indicate if the certificate has been validated.
 *
 *  :message ::
 *    The message to insert into the reflogs. Defaults to "fetch".
 *
 *  :prune ::
 *    Specifies the prune mode for the fetch. +true+ remove any remote-tracking references that
 *    no longer exist, +false+ do not prune, +nil+ use configured settings Defaults to "nil".
 *
 *  :proxy_url ::
 *    The url of an http proxy to use to access the remote repository.
 *
 *  Example:
 *
 *    remote = Rugged::Remote.lookup(@repo, 'origin')
 *    remote.fetch({
 *      transfer_progress: lambda { |total_objects, indexed_objects, received_objects, local_objects, total_deltas, indexed_deltas, received_bytes|
 *        # ...
 *      }
 *    })
 */
static VALUE rb_git_remote_fetch(int argc, VALUE *argv, VALUE self)
{
	git_remote *remote;
	git_strarray refspecs;
	git_fetch_options opts = GIT_FETCH_OPTIONS_INIT;
	const git_transfer_progress *stats;
	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, 0 };

	char *log_message = NULL;
	int error;

	VALUE rb_options, rb_refspecs, rb_result = Qnil;

	rb_scan_args(argc, argv, "01:", &rb_refspecs, &rb_options);

	rugged_rb_ary_to_strarray(rb_refspecs, &refspecs);

	Data_Get_Struct(self, git_remote, remote);

	rugged_remote_init_callbacks_and_payload_from_options(rb_options, &opts.callbacks, &payload);
	rugged_remote_init_custom_headers(rb_options, &opts.custom_headers);
	rugged_remote_init_proxy_options(rb_options, &opts.proxy_opts);

	if (!NIL_P(rb_options)) {
		VALUE rb_prune_type;
		VALUE rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));

		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);

		rb_prune_type = rb_hash_aref(rb_options, CSTR2SYM("prune"));
		opts.prune = parse_prune_type(rb_prune_type);
	}

	error = git_remote_fetch(remote, &refspecs, &opts, log_message);

	xfree(refspecs.strings);
	xfree(opts.custom_headers.strings);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	rugged_exception_check(error);

	stats = git_remote_stats(remote);

	rb_result = rb_hash_new();
	rb_hash_aset(rb_result, CSTR2SYM("total_objects"),    UINT2NUM(stats->total_objects));
	rb_hash_aset(rb_result, CSTR2SYM("indexed_objects"),  UINT2NUM(stats->indexed_objects));
	rb_hash_aset(rb_result, CSTR2SYM("received_objects"), UINT2NUM(stats->received_objects));
	rb_hash_aset(rb_result, CSTR2SYM("local_objects"),    UINT2NUM(stats->local_objects));
	rb_hash_aset(rb_result, CSTR2SYM("total_deltas"),     UINT2NUM(stats->total_deltas));
	rb_hash_aset(rb_result, CSTR2SYM("indexed_deltas"),   UINT2NUM(stats->indexed_deltas));
	rb_hash_aset(rb_result, CSTR2SYM("received_bytes"),   INT2FIX(stats->received_bytes));

	return rb_result;
}

/*
 *  call-seq:
 *    remote.push(refspecs = nil, options = {}) -> hash
 *
 *  Pushes the given +refspecs+ to the given +remote+. Returns a hash that contains
 *  key-value pairs that reflect pushed refs and error messages, if applicable.
 *
 *  You can optionally pass in an alternative list of +refspecs+ to use instead of the push
 *  refspecs already configured for +remote+.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the push operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :update_tips ::
 *    A callback that will be executed each time a reference is updated remotely. It will be
 *    passed the +refname+, +old_oid+ and +new_oid+.
 *
 *  :headers ::
 *    Extra HTTP headers to include with the push (only applies to http:// or https:// remotes)
 *
 *  :proxy_url ::
 *    The url of an http proxy to use to access the remote repository.
 *
 *  :pb_parallelism ::
 *    Sets the number of worker threads that will be available to the packbuilder when generating
 *    a pack file, if required by the transport being used. A value of 0 means the packbuilder will
 *    auto-detect the number of of threads to use.
 *
 *  Example:
 *
 *    remote = Rugged::Remote.lookup(@repo, 'origin')
 *    remote.push(["refs/heads/master", ":refs/heads/to_be_deleted"])
 */
static VALUE rb_git_remote_push(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_refspecs, rb_options;

	git_remote *remote;
	git_strarray refspecs;
	git_push_options opts = GIT_PUSH_OPTIONS_INIT;

	int error = 0;

	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, rb_hash_new(), 0 };

	rb_scan_args(argc, argv, "01:", &rb_refspecs, &rb_options);

	rugged_rb_ary_to_strarray(rb_refspecs, &refspecs);

	Data_Get_Struct(self, git_remote, remote);

	rugged_remote_init_callbacks_and_payload_from_options(rb_options, &opts.callbacks, &payload);
	rugged_remote_init_custom_headers(rb_options, &opts.custom_headers);
	rugged_remote_init_proxy_options(rb_options, &opts.proxy_opts);
	init_pb_parallelism(rb_options, &opts);

	error = git_remote_push(remote, &refspecs, &opts);

	xfree(refspecs.strings);
	xfree(opts.custom_headers.strings);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	rugged_exception_check(error);

	return payload.result;
}

void Init_rugged_remote(void)
{
	rb_cRuggedRemote = rb_define_class_under(rb_mRugged, "Remote", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedRemote);

	rb_define_method(rb_cRuggedRemote, "name", rb_git_remote_name, 0);
	rb_define_method(rb_cRuggedRemote, "url", rb_git_remote_url, 0);
	rb_define_method(rb_cRuggedRemote, "push_url", rb_git_remote_push_url, 0);
	rb_define_method(rb_cRuggedRemote, "push_url=", rb_git_remote_set_push_url, 1);
	rb_define_method(rb_cRuggedRemote, "fetch_refspecs", rb_git_remote_fetch_refspecs, 0);
	rb_define_method(rb_cRuggedRemote, "push_refspecs", rb_git_remote_push_refspecs, 0);
	rb_define_method(rb_cRuggedRemote, "ls", rb_git_remote_ls, -1);
	rb_define_method(rb_cRuggedRemote, "check_connection", rb_git_remote_check_connection, -1);
	rb_define_method(rb_cRuggedRemote, "fetch", rb_git_remote_fetch, -1);
	rb_define_method(rb_cRuggedRemote, "push", rb_git_remote_push, -1);
}
