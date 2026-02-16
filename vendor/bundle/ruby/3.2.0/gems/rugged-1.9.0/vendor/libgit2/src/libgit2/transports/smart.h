/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_transports_smart_h__
#define INCLUDE_transports_smart_h__

#include "common.h"

#include "git2.h"
#include "vector.h"
#include "push.h"
#include "str.h"
#include "oidarray.h"
#include "staticstr.h"
#include "git2/sys/transport.h"

#define GIT_SMART_BUFFER_SIZE  65536

#define GIT_SIDE_BAND_DATA     1
#define GIT_SIDE_BAND_PROGRESS 2
#define GIT_SIDE_BAND_ERROR    3

#define GIT_CAP_OFS_DELTA "ofs-delta"
#define GIT_CAP_MULTI_ACK "multi_ack"
#define GIT_CAP_MULTI_ACK_DETAILED "multi_ack_detailed"
#define GIT_CAP_SIDE_BAND "side-band"
#define GIT_CAP_SIDE_BAND_64K "side-band-64k"
#define GIT_CAP_INCLUDE_TAG "include-tag"
#define GIT_CAP_DELETE_REFS "delete-refs"
#define GIT_CAP_REPORT_STATUS "report-status"
#define GIT_CAP_THIN_PACK "thin-pack"
#define GIT_CAP_SYMREF "symref"
#define GIT_CAP_WANT_TIP_SHA1 "allow-tip-sha1-in-want"
#define GIT_CAP_WANT_REACHABLE_SHA1 "allow-reachable-sha1-in-want"
#define GIT_CAP_SHALLOW "shallow"
#define GIT_CAP_OBJECT_FORMAT "object-format="
#define GIT_CAP_AGENT "agent="
#define GIT_CAP_PUSH_OPTIONS "push-options"

extern bool git_smart__ofs_delta_enabled;

typedef enum {
	GIT_PKT_CMD,
	GIT_PKT_FLUSH,
	GIT_PKT_REF,
	GIT_PKT_HAVE,
	GIT_PKT_ACK,
	GIT_PKT_NAK,
	GIT_PKT_COMMENT,
	GIT_PKT_ERR,
	GIT_PKT_DATA,
	GIT_PKT_PROGRESS,
	GIT_PKT_OK,
	GIT_PKT_NG,
	GIT_PKT_UNPACK,
	GIT_PKT_SHALLOW,
	GIT_PKT_UNSHALLOW
} git_pkt_type;

/* Used for multi_ack and multi_ack_detailed */
enum git_ack_status {
	GIT_ACK_NONE,
	GIT_ACK_CONTINUE,
	GIT_ACK_COMMON,
	GIT_ACK_READY
};

/* This would be a flush pkt */
typedef struct {
	git_pkt_type type;
} git_pkt;

struct git_pkt_cmd {
	git_pkt_type type;
	char *cmd;
	char *path;
	char *host;
};

/* This is a pkt-line with some info in it */
typedef struct {
	git_pkt_type type;
	git_remote_head head;
	char *capabilities;
} git_pkt_ref;

/* Useful later */
typedef struct {
	git_pkt_type type;
	git_oid oid;
	enum git_ack_status status;
} git_pkt_ack;

typedef struct {
	git_pkt_type type;
	char comment[GIT_FLEX_ARRAY];
} git_pkt_comment;

typedef struct {
	git_pkt_type type;
	size_t len;
	char data[GIT_FLEX_ARRAY];
} git_pkt_data;

typedef git_pkt_data git_pkt_progress;

typedef struct {
	git_pkt_type type;
	size_t len;
	char error[GIT_FLEX_ARRAY];
} git_pkt_err;

typedef struct {
	git_pkt_type type;
	char *ref;
} git_pkt_ok;

typedef struct {
	git_pkt_type type;
	char *ref;
	char *msg;
} git_pkt_ng;

typedef struct {
	git_pkt_type type;
	int unpack_ok;
} git_pkt_unpack;

typedef struct {
	git_pkt_type type;
	git_oid oid;
} git_pkt_shallow;

typedef struct transport_smart_caps {
	unsigned int common:1,
	             ofs_delta:1,
	             multi_ack:1,
	             multi_ack_detailed:1,
	             side_band:1,
	             side_band_64k:1,
	             include_tag:1,
	             delete_refs:1,
	             report_status:1,
	             thin_pack:1,
	             want_tip_sha1:1,
	             want_reachable_sha1:1,
	             shallow:1,
	             push_options:1;
	char *object_format;
	char *agent;
} transport_smart_caps;

typedef int (*packetsize_cb)(size_t received, void *payload);

typedef struct {
	git_transport parent;
	git_remote *owner;
	char *url;
	git_remote_connect_options connect_opts;
	int direction;
	git_smart_subtransport *wrapped;
	git_smart_subtransport_stream *current_stream;
	transport_smart_caps caps;
	git_vector refs;
	git_vector heads;
	git_vector common;
	git_array_oid_t shallow_roots;
	git_atomic32 cancelled;
	packetsize_cb packetsize_cb;
	void *packetsize_payload;
	unsigned rpc : 1,
	         have_refs : 1,
	         connected : 1;
	git_staticstr_with_size(GIT_SMART_BUFFER_SIZE) buffer;
} transport_smart;

/* smart_protocol.c */
int git_smart__store_refs(transport_smart *t, int flushes);
int git_smart__detect_caps(git_pkt_ref *pkt, transport_smart_caps *caps, git_vector *symrefs);
int git_smart__push(git_transport *transport, git_push *push);

int git_smart__negotiate_fetch(
	git_transport *transport,
	git_repository *repo,
	const git_fetch_negotiation *wants);

int git_smart__shallow_roots(git_oidarray *out, git_transport *transport);

int git_smart__download_pack(
	git_transport *transport,
	git_repository *repo,
	git_indexer_progress *stats);

/* smart.c */
int git_smart__recv(transport_smart *t);

int git_smart__negotiation_step(git_transport *transport, void *data, size_t len);
int git_smart__get_push_stream(transport_smart *t, git_smart_subtransport_stream **out);

int git_smart__update_heads(transport_smart *t, git_vector *symrefs);

/* smart_pkt.c */
typedef struct {
	git_oid_t oid_type;
	unsigned int seen_capabilities: 1;
} git_pkt_parse_data;

int git_pkt_parse_line(git_pkt **head, const char **endptr, const char *line, size_t linelen, git_pkt_parse_data *data);
int git_pkt_buffer_flush(git_str *buf);
int git_pkt_send_flush(GIT_SOCKET s);
int git_pkt_buffer_done(git_str *buf);
int git_pkt_buffer_wants(const git_fetch_negotiation *wants, transport_smart_caps *caps, git_str *buf);
int git_pkt_buffer_have(git_oid *oid, git_str *buf);
void git_pkt_free(git_pkt *pkt);

#endif
