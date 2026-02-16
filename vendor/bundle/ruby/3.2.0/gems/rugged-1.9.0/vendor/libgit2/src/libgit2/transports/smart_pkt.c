/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "smart.h"
#include "util.h"
#include "posix.h"
#include "str.h"
#include "oid.h"

#include "git2/types.h"
#include "git2/errors.h"
#include "git2/refs.h"
#include "git2/revwalk.h"

#include <ctype.h>

#define PKT_DONE_STR    "0009done\n"
#define PKT_FLUSH_STR   "0000"
#define PKT_HAVE_PREFIX "have "
#define PKT_WANT_PREFIX "want "

#define PKT_LEN_SIZE    4
#define PKT_MAX_SIZE    0xffff
#define PKT_MAX_WANTLEN (PKT_LEN_SIZE + CONST_STRLEN(PKT_WANT_PREFIX) + GIT_OID_MAX_HEXSIZE + 1)

static int flush_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_FLUSH;
	*out = pkt;

	return 0;
}

/* the rest of the line will be useful for multi_ack and multi_ack_detailed */
static int ack_pkt(
	git_pkt **out,
	const char *line,
	size_t len,
	git_pkt_parse_data *data)
{
	git_pkt_ack *pkt;
	size_t oid_hexsize = git_oid_hexsize(data->oid_type);

	GIT_ASSERT(data && data->oid_type);

	pkt = git__calloc(1, sizeof(git_pkt_ack));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_ACK;

	if (git__prefixncmp(line, len, "ACK "))
		goto out_err;
	line += 4;
	len -= 4;

	if (len < oid_hexsize ||
	    git_oid__fromstr(&pkt->oid, line, data->oid_type) < 0)
		goto out_err;
	line += oid_hexsize;
	len -= oid_hexsize;

	if (len && line[0] == ' ') {
		line++;
		len--;

		if (!git__prefixncmp(line, len, "continue"))
			pkt->status = GIT_ACK_CONTINUE;
		else if (!git__prefixncmp(line, len, "common"))
			pkt->status = GIT_ACK_COMMON;
		else if (!git__prefixncmp(line, len, "ready"))
			pkt->status = GIT_ACK_READY;
		else
			goto out_err;
	}

	*out = (git_pkt *) pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing ACK pkt-line");
	git__free(pkt);
	return -1;
}

static int nak_pkt(git_pkt **out)
{
	git_pkt *pkt;

	pkt = git__malloc(sizeof(git_pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_NAK;
	*out = pkt;

	return 0;
}

static int comment_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_comment *pkt;
	size_t alloclen;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_comment), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_COMMENT;
	memcpy(pkt->comment, line, len);
	pkt->comment[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;
}

static int err_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt = NULL;
	size_t alloclen;

	/* Remove "ERR " from the line */
	if (git__prefixncmp(line, len, "ERR "))
		goto out_err;
	line += 4;
	len -= 4;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_ERR;
	pkt->len = len;

	memcpy(pkt->error, line, len);
	pkt->error[len] = '\0';

	*out = (git_pkt *) pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing ERR pkt-line");
	git__free(pkt);
	return -1;
}

static int data_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_data *pkt;
	size_t alloclen;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_DATA;
	pkt->len = len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_progress_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_progress *pkt;
	size_t alloclen;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_pkt_progress), len);
	pkt = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_PROGRESS;
	pkt->len = len;
	memcpy(pkt->data, line, len);

	*out = (git_pkt *) pkt;

	return 0;
}

static int sideband_error_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_err *pkt;
	size_t alloc_len;

	line++;
	len--;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, sizeof(git_pkt_err), len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 1);
	pkt = git__malloc(alloc_len);
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_ERR;
	pkt->len = (int)len;
	memcpy(pkt->error, line, len);
	pkt->error[len] = '\0';

	*out = (git_pkt *)pkt;

	return 0;
}

static int set_data(
	git_pkt_parse_data *data,
	const char *line,
	size_t len)
{
	const char *caps, *format_str = NULL, *eos;
	size_t format_len;
	git_oid_t remote_oid_type;

	GIT_ASSERT_ARG(data);

	if ((caps = memchr(line, '\0', len)) != NULL &&
	    len > (size_t)((caps - line) + 1)) {
		caps++;

		if (strncmp(caps, "object-format=", CONST_STRLEN("object-format=")) == 0)
			format_str = caps + CONST_STRLEN("object-format=");
		else if ((format_str = strstr(caps, " object-format=")) != NULL)
			format_str += CONST_STRLEN(" object-format=");
	}

	if (format_str) {
		if ((eos = strchr(format_str, ' ')) == NULL)
			eos = strchr(format_str, '\0');

		GIT_ASSERT(eos);

		format_len = eos - format_str;

		if ((remote_oid_type = git_oid_type_fromstrn(format_str, format_len)) == 0) {
			git_error_set(GIT_ERROR_INVALID, "unknown remote object format '%.*s'", (int)format_len, format_str);
			return -1;
		}
	} else {
		remote_oid_type = GIT_OID_SHA1;
	}

	if (!data->oid_type) {
		data->oid_type = remote_oid_type;
	} else if (data->oid_type != remote_oid_type) {
		git_error_set(GIT_ERROR_INVALID,
		              "the local object format '%s' does not match the remote object format '%s'",
		              git_oid_type_name(data->oid_type),
		              git_oid_type_name(remote_oid_type));
		return -1;
	}

	return 0;
}

/*
 * Parse an other-ref line.
 */
static int ref_pkt(
	git_pkt **out,
	const char *line,
	size_t len,
	git_pkt_parse_data *data)
{
	git_pkt_ref *pkt;
	size_t alloclen, oid_hexsize;

	pkt = git__calloc(1, sizeof(git_pkt_ref));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_REF;

	/* Determine OID type from capabilities */
	if (!data->seen_capabilities && set_data(data, line, len) < 0)
		return -1;

	GIT_ASSERT(data->oid_type);
	oid_hexsize = git_oid_hexsize(data->oid_type);

	if (len < oid_hexsize ||
	    git_oid__fromstr(&pkt->head.oid, line, data->oid_type) < 0)
		goto out_err;
	line += oid_hexsize;
	len -= oid_hexsize;

	if (git__prefixncmp(line, len, " "))
		goto out_err;

	line++;
	len--;

	if (!len)
		goto out_err;

	if (line[len - 1] == '\n')
		--len;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->head.name = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->head.name);

	memcpy(pkt->head.name, line, len);
	pkt->head.name[len] = '\0';

	if (strlen(pkt->head.name) < len) {
		if (!data->seen_capabilities)
			pkt->capabilities = strchr(pkt->head.name, '\0') + 1;
		else
			goto out_err;
	}

	data->seen_capabilities = 1;

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing REF pkt-line");
	if (pkt)
		git__free(pkt->head.name);
	git__free(pkt);
	return -1;
}

static int ok_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ok *pkt;
	size_t alloc_len;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_OK;

	if (git__prefixncmp(line, len, "ok "))
		goto out_err;
	line += 3;
	len -= 3;

	if (len && line[len - 1] == '\n')
		--len;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, len, 1);
	pkt->ref = git__malloc(alloc_len);
	GIT_ERROR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "error parsing OK pkt-line");
	git__free(pkt);
	return -1;
}

static int ng_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_ng *pkt;
	const char *ptr, *eol;
	size_t alloclen;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->ref = NULL;
	pkt->type = GIT_PKT_NG;

	eol = line + len;

	if (git__prefixncmp(line, len, "ng "))
		goto out_err;
	line += 3;

	if (!(ptr = memchr(line, ' ', eol - line)))
		goto out_err;
	len = ptr - line;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->ref = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->ref);

	memcpy(pkt->ref, line, len);
	pkt->ref[len] = '\0';

	line = ptr + 1;
	if (line >= eol)
		goto out_err;

	if (!(ptr = memchr(line, '\n', eol - line)))
		goto out_err;
	len = ptr - line;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
	pkt->msg = git__malloc(alloclen);
	GIT_ERROR_CHECK_ALLOC(pkt->msg);

	memcpy(pkt->msg, line, len);
	pkt->msg[len] = '\0';

	*out = (git_pkt *)pkt;
	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "invalid packet line");
	git__free(pkt->ref);
	git__free(pkt);
	return -1;
}

static int unpack_pkt(git_pkt **out, const char *line, size_t len)
{
	git_pkt_unpack *pkt;

	pkt = git__malloc(sizeof(*pkt));
	GIT_ERROR_CHECK_ALLOC(pkt);
	pkt->type = GIT_PKT_UNPACK;

	if (!git__prefixncmp(line, len, "unpack ok"))
		pkt->unpack_ok = 1;
	else
		pkt->unpack_ok = 0;

	*out = (git_pkt *)pkt;
	return 0;
}

static int shallow_pkt(
	git_pkt **out,
	const char *line,
	size_t len,
	git_pkt_parse_data *data)
{
	git_pkt_shallow *pkt;
	size_t oid_hexsize = git_oid_hexsize(data->oid_type);

	GIT_ASSERT(data && data->oid_type);

	pkt = git__calloc(1, sizeof(git_pkt_shallow));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_SHALLOW;

	if (git__prefixncmp(line, len, "shallow "))
		goto out_err;

	line += 8;
	len -= 8;

	if (len != oid_hexsize)
		goto out_err;

	git_oid__fromstr(&pkt->oid, line, data->oid_type);
	line += oid_hexsize + 1;
	len -= oid_hexsize + 1;

	*out = (git_pkt *)pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "invalid packet line");
	git__free(pkt);
	return -1;
}

static int unshallow_pkt(
	git_pkt **out,
	const char *line,
	size_t len,
	git_pkt_parse_data *data)
{
	git_pkt_shallow *pkt;
	size_t oid_hexsize = git_oid_hexsize(data->oid_type);

	GIT_ASSERT(data && data->oid_type);

	pkt = git__calloc(1, sizeof(git_pkt_shallow));
	GIT_ERROR_CHECK_ALLOC(pkt);

	pkt->type = GIT_PKT_UNSHALLOW;

	if (git__prefixncmp(line, len, "unshallow "))
		goto out_err;

	line += 10;
	len -= 10;

	if (len != oid_hexsize)
		goto out_err;

	git_oid__fromstr(&pkt->oid, line, data->oid_type);
	line += oid_hexsize + 1;
	len -= oid_hexsize + 1;

	*out = (git_pkt *) pkt;

	return 0;

out_err:
	git_error_set(GIT_ERROR_NET, "invalid packet line");
	git__free(pkt);
	return -1;
}

static int parse_len(size_t *out, const char *line, size_t linelen)
{
	char num[PKT_LEN_SIZE + 1];
	int i, k, error;
	int32_t len;
	const char *num_end;

	/* Not even enough for the length */
	if (linelen < PKT_LEN_SIZE)
		return GIT_EBUFS;

	memcpy(num, line, PKT_LEN_SIZE);
	num[PKT_LEN_SIZE] = '\0';

	for (i = 0; i < PKT_LEN_SIZE; ++i) {
		if (!git__isxdigit(num[i])) {
			/* Make sure there are no special characters before passing to error message */
			for (k = 0; k < PKT_LEN_SIZE; ++k) {
				if(!git__isprint(num[k])) {
					num[k] = '.';
				}
			}

			git_error_set(GIT_ERROR_NET, "invalid hex digit in length: '%s'", num);
			return -1;
		}
	}

	if ((error = git__strntol32(&len, num, PKT_LEN_SIZE, &num_end, 16)) < 0)
		return error;

	if (len < 0)
		return -1;

	*out = (size_t) len;
	return 0;
}

/*
 * As per the documentation, the syntax is:
 *
 * pkt-line	= data-pkt / flush-pkt
 * data-pkt	= pkt-len pkt-payload
 * pkt-len		= 4*(HEXDIG)
 * pkt-payload = (pkt-len -4)*(OCTET)
 * flush-pkt	= "0000"
 *
 * Which means that the first four bytes are the length of the line,
 * in ASCII hexadecimal (including itself)
 */

int git_pkt_parse_line(
	git_pkt **pkt,
	const char **endptr,
	const char *line,
	size_t linelen,
	git_pkt_parse_data *data)
{
	int error;
	size_t len;

	if ((error = parse_len(&len, line, linelen)) < 0) {
		/*
		 * If we fail to parse the length, it might be
		 * because the server is trying to send us the
		 * packfile already or because we do not yet have
		 * enough data.
		 */
		if (error == GIT_EBUFS)
			;
		else if (!git__prefixncmp(line, linelen, "PACK"))
			git_error_set(GIT_ERROR_NET, "unexpected pack file");
		else
			git_error_set(GIT_ERROR_NET, "bad packet length");
		return error;
	}

	/*
	 * Make sure there is enough in the buffer to satisfy
	 * this line.
	 */
	if (linelen < len)
		return GIT_EBUFS;

	/*
	 * The length has to be exactly 0 in case of a flush
	 * packet or greater than PKT_LEN_SIZE, as the decoded
	 * length includes its own encoded length of four bytes.
	 */
	if (len != 0 && len < PKT_LEN_SIZE)
		return GIT_ERROR;

	line += PKT_LEN_SIZE;
	/*
	 * The Git protocol does not specify empty lines as part
	 * of the protocol. Not knowing what to do with an empty
	 * line, we should return an error upon hitting one.
	 */
	if (len == PKT_LEN_SIZE) {
		git_error_set_str(GIT_ERROR_NET, "Invalid empty packet");
		return GIT_ERROR;
	}

	if (len == 0) { /* Flush pkt */
		*endptr = line;
		return flush_pkt(pkt);
	}

	len -= PKT_LEN_SIZE; /* the encoded length includes its own size */

	if (*line == GIT_SIDE_BAND_DATA)
		error = data_pkt(pkt, line, len);
	else if (*line == GIT_SIDE_BAND_PROGRESS)
		error = sideband_progress_pkt(pkt, line, len);
	else if (*line == GIT_SIDE_BAND_ERROR)
		error = sideband_error_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ACK"))
		error = ack_pkt(pkt, line, len, data);
	else if (!git__prefixncmp(line, len, "NAK"))
		error = nak_pkt(pkt);
	else if (!git__prefixncmp(line, len, "ERR"))
		error = err_pkt(pkt, line, len);
	else if (*line == '#')
		error = comment_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ok"))
		error = ok_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "ng"))
		error = ng_pkt(pkt, line, len);
	else if (!git__prefixncmp(line, len, "unpack"))
		error = unpack_pkt(pkt, line, len);
	else if (!git__prefixcmp(line, "shallow"))
		error = shallow_pkt(pkt, line, len, data);
	else if (!git__prefixcmp(line, "unshallow"))
		error = unshallow_pkt(pkt, line, len, data);
	else
		error = ref_pkt(pkt, line, len, data);

	*endptr = line + len;

	return error;
}

void git_pkt_free(git_pkt *pkt)
{
	if (pkt == NULL) {
		return;
	}
	if (pkt->type == GIT_PKT_REF) {
		git_pkt_ref *p = (git_pkt_ref *) pkt;
		git__free(p->head.name);
		git__free(p->head.symref_target);
	}

	if (pkt->type == GIT_PKT_OK) {
		git_pkt_ok *p = (git_pkt_ok *) pkt;
		git__free(p->ref);
	}

	if (pkt->type == GIT_PKT_NG) {
		git_pkt_ng *p = (git_pkt_ng *) pkt;
		git__free(p->ref);
		git__free(p->msg);
	}

	git__free(pkt);
}

int git_pkt_buffer_flush(git_str *buf)
{
	return git_str_put(buf, PKT_FLUSH_STR, CONST_STRLEN(PKT_FLUSH_STR));
}

static int buffer_want_with_caps(
	const git_remote_head *head,
	transport_smart_caps *caps,
	git_oid_t oid_type,
	git_str *buf)
{
	git_str str = GIT_STR_INIT;
	char oid[GIT_OID_MAX_HEXSIZE];
	size_t oid_hexsize, len;

	oid_hexsize = git_oid_hexsize(oid_type);
	git_oid_fmt(oid, &head->oid);

	/* Prefer multi_ack_detailed */
	if (caps->multi_ack_detailed)
		git_str_puts(&str, GIT_CAP_MULTI_ACK_DETAILED " ");
	else if (caps->multi_ack)
		git_str_puts(&str, GIT_CAP_MULTI_ACK " ");

	/* Prefer side-band-64k if the server supports both */
	if (caps->side_band_64k)
		git_str_printf(&str, "%s ", GIT_CAP_SIDE_BAND_64K);
	else if (caps->side_band)
		git_str_printf(&str, "%s ", GIT_CAP_SIDE_BAND);

	if (caps->include_tag)
		git_str_puts(&str, GIT_CAP_INCLUDE_TAG " ");

	if (caps->thin_pack)
		git_str_puts(&str, GIT_CAP_THIN_PACK " ");

	if (caps->ofs_delta)
		git_str_puts(&str, GIT_CAP_OFS_DELTA " ");

	if (caps->shallow)
		git_str_puts(&str, GIT_CAP_SHALLOW " ");

	if (git_str_oom(&str))
		return -1;

	if (str.size > (PKT_MAX_SIZE - (PKT_MAX_WANTLEN + 1))) {
		git_error_set(GIT_ERROR_NET,
			"tried to produce packet with invalid caps length %" PRIuZ, str.size);
		return -1;
	}

	len = PKT_LEN_SIZE + CONST_STRLEN(PKT_WANT_PREFIX) +
	      oid_hexsize + 1 /* NUL */ +
	      git_str_len(&str) + 1 /* LF */;

	git_str_grow_by(buf, len);
	git_str_printf(buf,
		"%04x%s%.*s %s\n", (unsigned int)len, PKT_WANT_PREFIX,
		(int)oid_hexsize, oid, git_str_cstr(&str));
	git_str_dispose(&str);

	GIT_ERROR_CHECK_ALLOC_STR(buf);

	return 0;
}

/*
 * All "want" packets have the same length and format, so what we do
 * is overwrite the OID each time.
 */

int git_pkt_buffer_wants(
	const git_fetch_negotiation *wants,
	transport_smart_caps *caps,
	git_str *buf)
{
	const git_remote_head *head;
	char oid[GIT_OID_MAX_HEXSIZE];
	git_oid_t oid_type;
	size_t oid_hexsize, want_len, i = 0;

#ifdef GIT_EXPERIMENTAL_SHA256
	oid_type = wants->refs_len > 0 ? wants->refs[0]->oid.type : GIT_OID_SHA1;
#else
	oid_type = GIT_OID_SHA1;
#endif

	oid_hexsize = git_oid_hexsize(oid_type);

	want_len = PKT_LEN_SIZE + CONST_STRLEN(PKT_WANT_PREFIX) +
	      oid_hexsize + 1 /* LF */;

	if (caps->common) {
		for (; i < wants->refs_len; ++i) {
			head = wants->refs[i];
			if (!head->local)
				break;
		}

		if (buffer_want_with_caps(wants->refs[i], caps, oid_type, buf) < 0)
			return -1;

		i++;
	}

	for (; i < wants->refs_len; ++i) {
		head = wants->refs[i];

		if (head->local)
			continue;

		git_oid_fmt(oid, &head->oid);

		git_str_printf(buf, "%04x%s%.*s\n",
			(unsigned int)want_len, PKT_WANT_PREFIX,
			(int)oid_hexsize, oid);

		if (git_str_oom(buf))
			return -1;
	}

	/* Tell the server about our shallow objects */
	for (i = 0; i < wants->shallow_roots_len; i++) {
		char oid[GIT_OID_MAX_HEXSIZE + 1];
		git_str shallow_buf = GIT_STR_INIT;

		git_oid_tostr(oid, GIT_OID_MAX_HEXSIZE + 1, &wants->shallow_roots[i]);
		git_str_puts(&shallow_buf, "shallow ");
		git_str_puts(&shallow_buf, oid);
		git_str_putc(&shallow_buf, '\n');

		git_str_printf(buf, "%04x%s", (unsigned int)git_str_len(&shallow_buf) + 4, git_str_cstr(&shallow_buf));

		git_str_dispose(&shallow_buf);

		if (git_str_oom(buf))
			return -1;
	}

	if (wants->depth > 0) {
		git_str deepen_buf = GIT_STR_INIT;

		git_str_printf(&deepen_buf, "deepen %d\n", wants->depth);
		git_str_printf(buf,"%04x%s", (unsigned int)git_str_len(&deepen_buf) + 4, git_str_cstr(&deepen_buf));

		git_str_dispose(&deepen_buf);

		if (git_str_oom(buf))
			return -1;
	}

	return git_pkt_buffer_flush(buf);
}

int git_pkt_buffer_have(git_oid *oid, git_str *buf)
{
	char oid_str[GIT_OID_MAX_HEXSIZE];
	git_oid_t oid_type;
	size_t oid_hexsize, have_len;

#ifdef GIT_EXPERIMENTAL_SHA256
	oid_type = oid->type;
#else
	oid_type = GIT_OID_SHA1;
#endif

	oid_hexsize = git_oid_hexsize(oid_type);
	have_len = PKT_LEN_SIZE + CONST_STRLEN(PKT_HAVE_PREFIX) +
	      oid_hexsize + 1 /* LF */;

	git_oid_fmt(oid_str, oid);
	return git_str_printf(buf, "%04x%s%.*s\n",
		(unsigned int)have_len, PKT_HAVE_PREFIX,
		(int)oid_hexsize, oid_str);
}

int git_pkt_buffer_done(git_str *buf)
{
	return git_str_put(buf, PKT_DONE_STR, CONST_STRLEN(PKT_DONE_STR));
}
