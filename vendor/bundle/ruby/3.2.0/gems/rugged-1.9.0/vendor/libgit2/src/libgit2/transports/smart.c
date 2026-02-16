/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "smart.h"

#include "git2.h"
#include "git2/sys/remote.h"
#include "refs.h"
#include "refspec.h"
#include "proxy.h"

int git_smart__recv(transport_smart *t)
{
	size_t bytes_read;
	int ret;

	GIT_ASSERT_ARG(t);
	GIT_ASSERT(t->current_stream);

	if (git_staticstr_remain(&t->buffer) == 0) {
		git_error_set(GIT_ERROR_NET, "out of buffer space");
		return -1;
	}

	ret = t->current_stream->read(t->current_stream,
		git_staticstr_offset(&t->buffer),
		git_staticstr_remain(&t->buffer),
		&bytes_read);

	if (ret < 0)
		return ret;

	GIT_ASSERT(bytes_read <= INT_MAX);
	GIT_ASSERT(bytes_read <= git_staticstr_remain(&t->buffer));

	git_staticstr_increase(&t->buffer, bytes_read);

	if (t->packetsize_cb && !t->cancelled.val) {
		ret = t->packetsize_cb(bytes_read, t->packetsize_payload);

		if (ret) {
			git_atomic32_set(&t->cancelled, 1);
			return GIT_EUSER;
		}
	}

	return (int)bytes_read;
}

GIT_INLINE(int) git_smart__reset_stream(transport_smart *t, bool close_subtransport)
{
	if (t->current_stream) {
		t->current_stream->free(t->current_stream);
		t->current_stream = NULL;
	}

	if (close_subtransport) {
		git__free(t->url);
		t->url = NULL;

		if (t->wrapped->close(t->wrapped) < 0)
			return -1;
	}

	git__free(t->caps.object_format);
	t->caps.object_format = NULL;

	git__free(t->caps.agent);
	t->caps.agent = NULL;

	return 0;
}

int git_smart__update_heads(transport_smart *t, git_vector *symrefs)
{
	size_t i;
	git_pkt *pkt;

	git_vector_clear(&t->heads);
	git_vector_foreach(&t->refs, i, pkt) {
		git_pkt_ref *ref = (git_pkt_ref *) pkt;
		if (pkt->type != GIT_PKT_REF)
			continue;

		if (symrefs) {
			git_refspec *spec;
			git_str buf = GIT_STR_INIT;
			size_t j;
			int error = 0;

			git_vector_foreach(symrefs, j, spec) {
				git_str_clear(&buf);
				if (git_refspec_src_matches(spec, ref->head.name) &&
				    !(error = git_refspec__transform(&buf, spec, ref->head.name))) {
					git__free(ref->head.symref_target);
					ref->head.symref_target = git_str_detach(&buf);
				}
			}

			git_str_dispose(&buf);

			if (error < 0)
				return error;
		}

		if (git_vector_insert(&t->heads, &ref->head) < 0)
			return -1;
	}

	return 0;
}

static void free_symrefs(git_vector *symrefs)
{
	git_refspec *spec;
	size_t i;

	git_vector_foreach(symrefs, i, spec) {
		git_refspec__dispose(spec);
		git__free(spec);
	}

	git_vector_dispose(symrefs);
}

static int git_smart__connect(
	git_transport *transport,
	const char *url,
	int direction,
	const git_remote_connect_options *connect_opts)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_smart_subtransport_stream *stream;
	int error;
	git_pkt *pkt;
	git_pkt_ref *first;
	git_vector symrefs;
	git_smart_service_t service;

	if (git_smart__reset_stream(t, true) < 0)
		return -1;

	if (git_remote_connect_options_normalize(&t->connect_opts, t->owner->repo, connect_opts) < 0)
		return -1;

	t->url = git__strdup(url);
	GIT_ERROR_CHECK_ALLOC(t->url);

	t->direction = direction;

	if (GIT_DIRECTION_FETCH == t->direction) {
		service = GIT_SERVICE_UPLOADPACK_LS;
	} else if (GIT_DIRECTION_PUSH == t->direction) {
		service = GIT_SERVICE_RECEIVEPACK_LS;
	} else {
		git_error_set(GIT_ERROR_NET, "invalid direction");
		return -1;
	}

	if ((error = t->wrapped->action(&stream, t->wrapped, t->url, service)) < 0)
		return error;

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = stream;

	/* 2 flushes for RPC; 1 for stateful */
	if ((error = git_smart__store_refs(t, t->rpc ? 2 : 1)) < 0)
		return error;

	/* Strip the comment packet for RPC */
	if (t->rpc) {
		pkt = (git_pkt *)git_vector_get(&t->refs, 0);

		if (!pkt || GIT_PKT_COMMENT != pkt->type) {
			git_error_set(GIT_ERROR_NET, "invalid response");
			return -1;
		} else {
			/* Remove the comment pkt from the list */
			git_vector_remove(&t->refs, 0);
			git__free(pkt);
		}
	}

	/* We now have loaded the refs. */
	t->have_refs = 1;

	pkt = (git_pkt *)git_vector_get(&t->refs, 0);
	if (pkt && GIT_PKT_REF != pkt->type) {
		git_error_set(GIT_ERROR_NET, "invalid response");
		return -1;
	}
	first = (git_pkt_ref *)pkt;

	if ((error = git_vector_init(&symrefs, 1, NULL)) < 0)
		return error;

	/* Detect capabilities */
	if ((error = git_smart__detect_caps(first, &t->caps, &symrefs)) == 0) {
		/* If the only ref in the list is capabilities^{} with OID_ZERO, remove it */
		if (1 == t->refs.length && !strcmp(first->head.name, "capabilities^{}") &&
			git_oid_is_zero(&first->head.oid)) {
			git_vector_clear(&t->refs);
			git_pkt_free((git_pkt *)first);
		}

		/* Keep a list of heads for _ls */
		git_smart__update_heads(t, &symrefs);
	} else if (error == GIT_ENOTFOUND) {
		/* There was no ref packet received, or the cap list was empty */
		error = 0;
	} else {
		git_error_set(GIT_ERROR_NET, "invalid response");
		goto cleanup;
	}

	if (t->rpc && (error = git_smart__reset_stream(t, false)) < 0)
		goto cleanup;

	/* We're now logically connected. */
	t->connected = 1;

cleanup:
	free_symrefs(&symrefs);

	return error;
}

static int git_smart__set_connect_opts(
	git_transport *transport,
	const git_remote_connect_options *opts)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	if (!t->connected) {
		git_error_set(GIT_ERROR_NET, "cannot reconfigure a transport that is not connected");
		return -1;
	}

	return git_remote_connect_options_normalize(&t->connect_opts, t->owner->repo, opts);
}

static int git_smart__capabilities(unsigned int *capabilities, git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	*capabilities = 0;

	if (t->caps.push_options)
		*capabilities |= GIT_REMOTE_CAPABILITY_PUSH_OPTIONS;

	if (t->caps.want_tip_sha1)
		*capabilities |= GIT_REMOTE_CAPABILITY_TIP_OID;

	if (t->caps.want_reachable_sha1)
		*capabilities |= GIT_REMOTE_CAPABILITY_REACHABLE_OID;

	return 0;
}

#ifdef GIT_EXPERIMENTAL_SHA256
static int git_smart__oid_type(git_oid_t *out, git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	*out = 0;

	if (t->caps.object_format == NULL) {
		*out = GIT_OID_DEFAULT;
	} else {
		*out = git_oid_type_fromstr(t->caps.object_format);

		if (!*out) {
			git_error_set(GIT_ERROR_INVALID,
				"unknown object format '%s'",
				t->caps.object_format);
			return -1;
		}
	}

	return 0;
}
#endif

static int git_smart__ls(const git_remote_head ***out, size_t *size, git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	if (!t->have_refs) {
		git_error_set(GIT_ERROR_NET, "the transport has not yet loaded the refs");
		return -1;
	}

	*out = (const git_remote_head **) t->heads.contents;
	*size = t->heads.length;

	return 0;
}

int git_smart__negotiation_step(git_transport *transport, void *data, size_t len)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_smart_subtransport_stream *stream;
	int error;

	if (t->rpc && git_smart__reset_stream(t, false) < 0)
		return -1;

	if (GIT_DIRECTION_FETCH != t->direction) {
		git_error_set(GIT_ERROR_NET, "this operation is only valid for fetch");
		return -1;
	}

	if ((error = t->wrapped->action(&stream, t->wrapped, t->url, GIT_SERVICE_UPLOADPACK)) < 0)
		return error;

	/* If this is a stateful implementation, the stream we get back should be the same */
	GIT_ASSERT(t->rpc || t->current_stream == stream);

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = stream;

	if ((error = stream->write(stream, (const char *)data, len)) < 0)
		return error;

	return 0;
}

int git_smart__get_push_stream(transport_smart *t, git_smart_subtransport_stream **stream)
{
	int error;

	if (t->rpc && git_smart__reset_stream(t, false) < 0)
		return -1;

	if (GIT_DIRECTION_PUSH != t->direction) {
		git_error_set(GIT_ERROR_NET, "this operation is only valid for push");
		return -1;
	}

	if ((error = t->wrapped->action(stream, t->wrapped, t->url, GIT_SERVICE_RECEIVEPACK)) < 0)
		return error;

	/* If this is a stateful implementation, the stream we get back should be the same */
	GIT_ASSERT(t->rpc || t->current_stream == *stream);

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = *stream;

	return 0;
}

static void git_smart__cancel(git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	git_atomic32_set(&t->cancelled, 1);
}

static int git_smart__is_connected(git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	return t->connected;
}

static int git_smart__close(git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_vector *common = &t->common;
	unsigned int i;
	git_pkt *p;
	git_smart_service_t service;
	int ret;
	git_smart_subtransport_stream *stream;
	const char flush[] = "0000";

	if (t->direction == GIT_DIRECTION_FETCH) {
		service = GIT_SERVICE_UPLOADPACK;
	} else if (t->direction == GIT_DIRECTION_PUSH) {
		service = GIT_SERVICE_RECEIVEPACK;
	} else {
		git_error_set(GIT_ERROR_NET, "invalid direction");
		return -1;
	}

	/*
	 * If we're still connected at this point and not using RPC,
	 * we should say goodbye by sending a flush, or git-daemon
	 * will complain that we disconnected unexpectedly.
	 */
	if (t->connected && !t->rpc &&
	    !t->wrapped->action(&stream, t->wrapped, t->url, service)) {
		t->current_stream->write(t->current_stream, flush, 4);
	}

	ret = git_smart__reset_stream(t, true);

	git_vector_foreach(common, i, p)
		git_pkt_free(p);

	git_vector_dispose(common);

	if (t->url) {
		git__free(t->url);
		t->url = NULL;
	}

	t->connected = 0;

	return ret;
}

static void git_smart__free(git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_vector *refs = &t->refs;
	unsigned int i;
	git_pkt *p;

	/* Make sure that the current stream is closed, if we have one. */
	git_smart__close(transport);

	/* Free the subtransport */
	t->wrapped->free(t->wrapped);

	git_vector_dispose(&t->heads);
	git_vector_foreach(refs, i, p)
		git_pkt_free(p);

	git_vector_dispose(refs);

	git_remote_connect_options_dispose(&t->connect_opts);

	git_array_dispose(t->shallow_roots);

	git__free(t->caps.object_format);
	git__free(t->caps.agent);
	git__free(t);
}

static int ref_name_cmp(const void *a, const void *b)
{
	const git_pkt_ref *ref_a = a, *ref_b = b;

	return strcmp(ref_a->head.name, ref_b->head.name);
}

int git_transport_smart_certificate_check(git_transport *transport, git_cert *cert, int valid, const char *hostname)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_remote_connect_options *connect_opts = &t->connect_opts;

	GIT_ASSERT_ARG(transport);
	GIT_ASSERT_ARG(cert);
	GIT_ASSERT_ARG(hostname);

	if (!connect_opts->callbacks.certificate_check)
		return GIT_PASSTHROUGH;

	return connect_opts->callbacks.certificate_check(cert, valid, hostname, connect_opts->callbacks.payload);
}

int git_transport_smart_credentials(git_credential **out, git_transport *transport, const char *user, int methods)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);
	git_remote_connect_options *connect_opts = &t->connect_opts;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(transport);

	if (!connect_opts->callbacks.credentials)
		return GIT_PASSTHROUGH;

	return connect_opts->callbacks.credentials(out, t->url, user, methods, connect_opts->callbacks.payload);
}

int git_transport_remote_connect_options(
		git_remote_connect_options *out,
		git_transport *transport)
{
	transport_smart *t = GIT_CONTAINER_OF(transport, transport_smart, parent);

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(transport);

	return git_remote_connect_options_dup(out, &t->connect_opts);
}

int git_transport_smart(git_transport **out, git_remote *owner, void *param)
{
	transport_smart *t;
	git_smart_subtransport_definition *definition = (git_smart_subtransport_definition *)param;

	if (!param)
		return -1;

	t = git__calloc(1, sizeof(transport_smart));
	GIT_ERROR_CHECK_ALLOC(t);

	t->parent.version = GIT_TRANSPORT_VERSION;
	t->parent.connect = git_smart__connect;
	t->parent.set_connect_opts = git_smart__set_connect_opts;
	t->parent.capabilities = git_smart__capabilities;
#ifdef GIT_EXPERIMENTAL_SHA256
	t->parent.oid_type = git_smart__oid_type;
#endif
	t->parent.close = git_smart__close;
	t->parent.free = git_smart__free;
	t->parent.negotiate_fetch = git_smart__negotiate_fetch;
	t->parent.shallow_roots = git_smart__shallow_roots;
	t->parent.download_pack = git_smart__download_pack;
	t->parent.push = git_smart__push;
	t->parent.ls = git_smart__ls;
	t->parent.is_connected = git_smart__is_connected;
	t->parent.cancel = git_smart__cancel;

	t->owner = owner;
	t->rpc = definition->rpc;

	if (git_vector_init(&t->refs, 16, ref_name_cmp) < 0 ||
	    git_vector_init(&t->heads, 16, ref_name_cmp) < 0 ||
	    definition->callback(&t->wrapped, &t->parent, definition->param) < 0) {
		git_vector_dispose(&t->refs);
		git_vector_dispose(&t->heads);
		git__free(t);
		return -1;
	}

	git_staticstr_init(&t->buffer, GIT_SMART_BUFFER_SIZE);

	*out = (git_transport *) t;
	return 0;
}
