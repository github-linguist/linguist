/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "push.h"

#include "git2.h"

#include "pack.h"
#include "pack-objects.h"
#include "remote.h"
#include "vector.h"
#include "tree.h"

static int push_spec_rref_cmp(const void *a, const void *b)
{
	const push_spec *push_spec_a = a, *push_spec_b = b;

	return strcmp(push_spec_a->refspec.dst, push_spec_b->refspec.dst);
}

static int push_status_ref_cmp(const void *a, const void *b)
{
	const push_status *push_status_a = a, *push_status_b = b;

	return strcmp(push_status_a->ref, push_status_b->ref);
}

int git_push_new(git_push **out, git_remote *remote, const git_push_options *opts)
{
	git_push *p;

	*out = NULL;

	GIT_ERROR_CHECK_VERSION(opts, GIT_PUSH_OPTIONS_VERSION, "git_push_options");

	p = git__calloc(1, sizeof(*p));
	GIT_ERROR_CHECK_ALLOC(p);

	p->repo = remote->repo;
	p->remote = remote;
	p->report_status = 1;
	p->pb_parallelism = opts ? opts->pb_parallelism : 1;

	if (opts) {
		GIT_ERROR_CHECK_VERSION(&opts->callbacks, GIT_REMOTE_CALLBACKS_VERSION, "git_remote_callbacks");
		memcpy(&p->callbacks, &opts->callbacks, sizeof(git_remote_callbacks));
	}

	if (git_vector_init(&p->specs, 0, push_spec_rref_cmp) < 0) {
		git__free(p);
		return -1;
	}

	if (git_vector_init(&p->status, 0, push_status_ref_cmp) < 0) {
		git_vector_dispose(&p->specs);
		git__free(p);
		return -1;
	}

	if (git_vector_init(&p->updates, 0, NULL) < 0) {
		git_vector_dispose(&p->status);
		git_vector_dispose(&p->specs);
		git__free(p);
		return -1;
	}

	if (git_vector_init(&p->remote_push_options, 0, git__strcmp_cb) < 0) {
		git_vector_dispose(&p->status);
		git_vector_dispose(&p->specs);
		git_vector_dispose(&p->updates);
		git__free(p);
		return -1;
	}

	*out = p;
	return 0;
}

static void free_refspec(push_spec *spec)
{
	if (spec == NULL)
		return;

	git_refspec__dispose(&spec->refspec);
	git__free(spec);
}

static int check_rref(char *ref)
{
	if (git__prefixcmp(ref, "refs/")) {
		git_error_set(GIT_ERROR_INVALID, "not a valid reference '%s'", ref);
		return -1;
	}

	return 0;
}

static int check_lref(git_push *push, char *ref)
{
	/* lref must be resolvable to an existing object */
	git_object *obj;
	int error = git_revparse_single(&obj, push->repo, ref);
	git_object_free(obj);

	if (!error)
		return 0;

	if (error == GIT_ENOTFOUND)
		git_error_set(GIT_ERROR_REFERENCE,
			"src refspec '%s' does not match any existing object", ref);
	else
		git_error_set(GIT_ERROR_INVALID, "not a valid reference '%s'", ref);
	return -1;
}

static int parse_refspec(git_push *push, push_spec **spec, const char *str)
{
	push_spec *s;

	*spec = NULL;

	s = git__calloc(1, sizeof(*s));
	GIT_ERROR_CHECK_ALLOC(s);

	git_oid_clear(&s->loid, push->repo->oid_type);
	git_oid_clear(&s->roid, push->repo->oid_type);

	if (git_refspec__parse(&s->refspec, str, false) < 0) {
		git_error_set(GIT_ERROR_INVALID, "invalid refspec %s", str);
		goto on_error;
	}

	if (s->refspec.src && s->refspec.src[0] != '\0' &&
	    check_lref(push, s->refspec.src) < 0) {
		goto on_error;
	}

	if (check_rref(s->refspec.dst) < 0)
		goto on_error;

	*spec = s;
	return 0;

on_error:
	free_refspec(s);
	return -1;
}

int git_push_add_refspec(git_push *push, const char *refspec)
{
	push_spec *spec;

	if (parse_refspec(push, &spec, refspec) < 0 ||
	    git_vector_insert(&push->specs, spec) < 0)
		return -1;

	return 0;
}

int git_push_update_tips(git_push *push, const git_remote_callbacks *callbacks)
{
	git_str remote_ref_name = GIT_STR_INIT;
	size_t i, j;
	git_refspec *fetch_spec;
	push_spec *push_spec = NULL;
	git_reference *remote_ref;
	push_status *status;
	int error = 0;

	git_vector_foreach(&push->status, i, status) {
		int fire_callback = 1;

		/* Skip unsuccessful updates which have non-empty messages */
		if (status->msg)
			continue;

		/* Find the corresponding remote ref */
		fetch_spec = git_remote__matching_refspec(push->remote, status->ref);
		if (!fetch_spec)
			continue;

		/* Clear the buffer which can be dirty from previous iteration */
		git_str_clear(&remote_ref_name);

		if ((error = git_refspec__transform(&remote_ref_name, fetch_spec, status->ref)) < 0)
			goto on_error;

		/* Find matching  push ref spec */
		git_vector_foreach(&push->specs, j, push_spec) {
			if (!strcmp(push_spec->refspec.dst, status->ref))
				break;
		}

		/* Could not find the corresponding push ref spec for this push update */
		if (j == push->specs.length)
			continue;

		/* Update the remote ref */
		if (git_oid_is_zero(&push_spec->loid)) {
			error = git_reference_lookup(&remote_ref, push->remote->repo, git_str_cstr(&remote_ref_name));

			if (error >= 0) {
				error = git_reference_delete(remote_ref);
				git_reference_free(remote_ref);
			}
		} else {
			error = git_reference_create(NULL, push->remote->repo,
						git_str_cstr(&remote_ref_name), &push_spec->loid, 1,
						"update by push");
		}

		if (error < 0) {
			if (error != GIT_ENOTFOUND)
				goto on_error;

			git_error_clear();
			fire_callback = 0;
		}

		if (!fire_callback || !callbacks)
			continue;

		if (callbacks->update_refs)
			error = callbacks->update_refs(
				git_str_cstr(&remote_ref_name),
				&push_spec->roid, &push_spec->loid,
				&push_spec->refspec, callbacks->payload);
#ifndef GIT_DEPRECATE_HARD
		else if (callbacks->update_tips)
			error = callbacks->update_tips(
				git_str_cstr(&remote_ref_name),
				&push_spec->roid, &push_spec->loid,
				callbacks->payload);
#endif

		if (error < 0) {
			git_error_set_after_callback_function(error, "git_remote_push");
			goto on_error;
		}
	}

	error = 0;

on_error:
	git_str_dispose(&remote_ref_name);
	return error;
}

/**
 * Insert all tags until we find a non-tag object, which is returned
 * in `out`.
 */
static int enqueue_tag(git_object **out, git_push *push, git_oid *id)
{
	git_object *obj = NULL, *target = NULL;
	int error;

	if ((error = git_object_lookup(&obj, push->repo, id, GIT_OBJECT_TAG)) < 0)
		return error;

	while (git_object_type(obj) == GIT_OBJECT_TAG) {
		if ((error = git_packbuilder_insert(push->pb, git_object_id(obj), NULL)) < 0)
			break;

		if ((error = git_tag_target(&target, (git_tag *) obj)) < 0)
			break;

		git_object_free(obj);
		obj = target;
	}

	if (error < 0)
		git_object_free(obj);
	else
		*out = obj;

	return error;
}

static int queue_objects(git_push *push)
{
	git_remote_head *head;
	push_spec *spec;
	git_revwalk *rw;
	unsigned int i;
	int error = -1;

	if (git_revwalk_new(&rw, push->repo) < 0)
		return -1;

	git_revwalk_sorting(rw, GIT_SORT_TIME);

	git_vector_foreach(&push->specs, i, spec) {
		git_object_t type;
		git_oid id;
		size_t size;

		if (git_oid_is_zero(&spec->loid))
			/*
			 * Delete reference on remote side;
			 * nothing to do here.
			 */
			continue;

		if (git_oid_equal(&spec->loid, &spec->roid))
			continue; /* up-to-date */

		if ((error = git_odb_read_header(&size, &type, push->repo->_odb, &spec->loid)) < 0)
			goto on_error;

		if (type == GIT_OBJECT_TAG) {
			git_object *target;

			if ((error = enqueue_tag(&target, push, &spec->loid)) < 0)
				goto on_error;

			type = git_object_type(target);
			git_oid_cpy(&id, git_object_id(target));

			git_object_free(target);
		} else {
			git_oid_cpy(&id, &spec->loid);
		}

		if (type == GIT_OBJECT_COMMIT)
			error = git_revwalk_push(rw, &id);
		else
			error = git_packbuilder_insert(push->pb, &id, NULL);

		if (error < 0)
			goto on_error;

		if (!spec->refspec.force) {
			git_oid base;

			if (git_oid_is_zero(&spec->roid))
				continue;

			if (!git_odb_exists(push->repo->_odb, &spec->roid)) {
				git_error_set(GIT_ERROR_REFERENCE,
					"cannot push because a reference that you are trying to update on the remote contains commits that are not present locally.");
				error = GIT_ENONFASTFORWARD;
				goto on_error;
			}

			error = git_merge_base(&base, push->repo,
					       &spec->loid, &spec->roid);

			if (error == GIT_ENOTFOUND ||
				(!error && !git_oid_equal(&base, &spec->roid))) {
				git_error_set(GIT_ERROR_REFERENCE,
					"cannot push non-fastforwardable reference");
				error = GIT_ENONFASTFORWARD;
				goto on_error;
			}

			if (error < 0)
				goto on_error;
		}
	}

	git_vector_foreach(&push->remote->refs, i, head) {
		if (git_oid_is_zero(&head->oid))
			continue;

		if ((error = git_revwalk_hide(rw, &head->oid)) < 0 &&
		    error != GIT_ENOTFOUND && error != GIT_EINVALIDSPEC && error != GIT_EPEEL)
			goto on_error;
	}

	error = git_packbuilder_insert_walk(push->pb, rw);

on_error:
	git_revwalk_free(rw);
	return error;
}

static int add_update(git_push *push, push_spec *spec)
{
	git_push_update *u = git__calloc(1, sizeof(git_push_update));
	GIT_ERROR_CHECK_ALLOC(u);

	u->src_refname = git__strdup(spec->refspec.src);
	GIT_ERROR_CHECK_ALLOC(u->src_refname);

	u->dst_refname = git__strdup(spec->refspec.dst);
	GIT_ERROR_CHECK_ALLOC(u->dst_refname);

	git_oid_cpy(&u->src, &spec->roid);
	git_oid_cpy(&u->dst, &spec->loid);

	return git_vector_insert(&push->updates, u);
}

static int calculate_work(git_push *push)
{
	git_remote_head *head;
	push_spec *spec;
	unsigned int i, j;

	/* Update local and remote oids*/

	git_vector_foreach(&push->specs, i, spec) {
		if (spec->refspec.src && spec->refspec.src[0]!= '\0') {
			/* This is a create or update.  Local ref must exist. */

			git_object *obj;
			int error = git_revparse_single(&obj, push->repo, spec->refspec.src);

			if (error < 0) {
				git_object_free(obj);
				git_error_set(GIT_ERROR_REFERENCE, "src refspec %s does not match any", spec->refspec.src);
				return -1;
			}

			git_oid_cpy(&spec->loid, git_object_id(obj));
			git_object_free(obj);
		}

		/* Remote ref may or may not (e.g. during create) already exist. */
		git_vector_foreach(&push->remote->refs, j, head) {
			if (!strcmp(spec->refspec.dst, head->name)) {
				git_oid_cpy(&spec->roid, &head->oid);
				break;
			}
		}

		if (add_update(push, spec) < 0)
			return -1;
	}

	return 0;
}

static int do_push(git_push *push)
{
	int error = 0;
	git_transport *transport = push->remote->transport;
	git_remote_callbacks *callbacks = &push->callbacks;

	if (!transport->push) {
		git_error_set(GIT_ERROR_NET, "remote transport doesn't support push");
		error = -1;
		goto on_error;
	}

	/*
	 * A pack-file MUST be sent if either create or update command
	 * is used, even if the server already has all the necessary
	 * objects.  In this case the client MUST send an empty pack-file.
	 */

	if ((error = git_packbuilder_new(&push->pb, push->repo)) < 0)
		goto on_error;

	git_packbuilder_set_threads(push->pb, push->pb_parallelism);

	if (callbacks && callbacks->pack_progress)
		if ((error = git_packbuilder_set_callbacks(push->pb, callbacks->pack_progress, callbacks->payload)) < 0)
			goto on_error;

	if ((error = calculate_work(push)) < 0)
		goto on_error;

	if (callbacks && callbacks->push_negotiation) {
		git_error_clear();

		error = callbacks->push_negotiation(
			(const git_push_update **) push->updates.contents,
			push->updates.length, callbacks->payload);

		if (error < 0) {
			git_error_set_after_callback_function(error,
				"push_negotiation");
			goto on_error;
		}

		error = 0;
	}

	if ((error = queue_objects(push)) < 0 ||
	    (error = transport->push(transport, push)) < 0)
		goto on_error;

on_error:
	git_packbuilder_free(push->pb);
	return error;
}

static int filter_refs(git_remote *remote)
{
	const git_remote_head **heads;
	size_t heads_len, i;

	git_vector_clear(&remote->refs);

	if (git_remote_ls(&heads, &heads_len, remote) < 0)
		return -1;

	for (i = 0; i < heads_len; i++) {
		if (git_vector_insert(&remote->refs, (void *)heads[i]) < 0)
			return -1;
	}

	return 0;
}

int git_push_finish(git_push *push)
{
	int error;
	unsigned int remote_caps;

	if (!git_remote_connected(push->remote)) {
		git_error_set(GIT_ERROR_NET, "remote is disconnected");
		return -1;
	}

	if ((error = git_remote_capabilities(&remote_caps, push->remote)) < 0) {
		git_error_set(GIT_ERROR_INVALID, "remote capabilities not available");
		return -1;
	}

	if (git_vector_length(&push->remote_push_options) > 0 &&
	    !(remote_caps & GIT_REMOTE_CAPABILITY_PUSH_OPTIONS)) {
		git_error_set(GIT_ERROR_INVALID, "push-options not supported by remote");
		return -1;
	}

	if ((error = filter_refs(push->remote)) < 0 ||
	    (error = do_push(push)) < 0)
		return error;

	if (!push->unpack_ok) {
		error = -1;
		git_error_set(GIT_ERROR_NET, "unpacking the sent packfile failed on the remote");
	}

	return error;
}

int git_push_status_foreach(git_push *push,
		int (*cb)(const char *ref, const char *msg, void *data),
		void *data)
{
	push_status *status;
	unsigned int i;

	git_vector_foreach(&push->status, i, status) {
		int error = cb(status->ref, status->msg, data);
		if (error)
			return git_error_set_after_callback(error);
	}

	return 0;
}

void git_push_status_free(push_status *status)
{
	if (status == NULL)
		return;

	git__free(status->msg);
	git__free(status->ref);
	git__free(status);
}

void git_push_free(git_push *push)
{
	push_spec *spec;
	push_status *status;
	git_push_update *update;
	char *option;
	unsigned int i;

	if (push == NULL)
		return;

	git_vector_foreach(&push->specs, i, spec) {
		free_refspec(spec);
	}
	git_vector_dispose(&push->specs);

	git_vector_foreach(&push->status, i, status) {
		git_push_status_free(status);
	}
	git_vector_dispose(&push->status);

	git_vector_foreach(&push->updates, i, update) {
		git__free(update->src_refname);
		git__free(update->dst_refname);
		git__free(update);
	}
	git_vector_dispose(&push->updates);

	git_vector_foreach(&push->remote_push_options, i, option) {
		git__free(option);
	}
	git_vector_dispose(&push->remote_push_options);

	git__free(push);
}

int git_push_options_init(git_push_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_push_options, GIT_PUSH_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_push_init_options(git_push_options *opts, unsigned int version)
{
	return git_push_options_init(opts, version);
}
#endif
