/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <arpa/inet.h>

#include "ntlm.h"
#include "unicode.h"
#include "utf8.h"
#include "crypt.h"
#include "compat.h"
#include "util.h"

#define NTLM_ASSERT_ARG(expr) do { \
		if (!(expr)) \
			return NTLM_CLIENT_ERROR_INVALID_INPUT; \
	} while(0)

#define NTLM_ASSERT(ntlm, expr) do { \
		if (!(expr)) { \
			ntlm_client_set_errmsg(ntlm, "internal error: " #expr); \
			return -1; \
		} \
	} while(0)

unsigned char ntlm_client_signature[] = NTLM_SIGNATURE;

static bool supports_unicode(ntlm_client *ntlm)
{
	return (ntlm->flags & NTLM_CLIENT_DISABLE_UNICODE) ?
		false : true;
}

NTLM_INLINE(bool) increment_size(size_t *out, size_t incr)
{
	if (SIZE_MAX - *out < incr) {
		*out = (size_t)-1;
		return false;
	}

	*out = *out + incr;
	return true;
}

ntlm_client *ntlm_client_init(ntlm_client_flags flags)
{
	ntlm_client *ntlm = NULL;

	if ((ntlm = calloc(1, sizeof(ntlm_client))) == NULL)
		return NULL;

	ntlm->flags = flags;

	return ntlm;
}

#define ENSURE_INITIALIZED(ntlm) \
	do { \
		if (!(ntlm)->unicode_initialized) \
			(ntlm)->unicode_initialized = ntlm_unicode_init((ntlm)); \
		if (!(ntlm)->crypt_initialized) \
			(ntlm)->crypt_initialized = ntlm_crypt_init((ntlm)); \
		if (!(ntlm)->unicode_initialized || \
		    !(ntlm)->crypt_initialized) \
			return -1; \
	} while(0)

void ntlm_client_set_errmsg(ntlm_client *ntlm, const char *errmsg)
{
	ntlm->state = NTLM_STATE_ERROR;
	ntlm->errmsg = errmsg;
}

const char *ntlm_client_errmsg(ntlm_client *ntlm)
{
	if (!ntlm)
		return "internal error";

	return ntlm->errmsg ? ntlm->errmsg : "no error";
}

int ntlm_client_set_version(
	ntlm_client *ntlm,
	uint8_t major,
	uint8_t minor,
	uint16_t build)
{
	NTLM_ASSERT_ARG(ntlm);

	ntlm->host_version.major = major;
	ntlm->host_version.minor = minor;
	ntlm->host_version.build = build;
	ntlm->host_version.reserved = 0x0f000000;

	ntlm->flags |= NTLM_ENABLE_HOSTVERSION;

	return 0;
}

#define reset(ptr) do { free(ptr); ptr = NULL; } while(0)

static void free_hostname(ntlm_client *ntlm)
{
	reset(ntlm->hostname);
	reset(ntlm->hostdomain);
	reset(ntlm->hostname_utf16);
	ntlm->hostname_utf16_len = 0;
}

int ntlm_client_set_hostname(
	ntlm_client *ntlm,
	const char *hostname,
	const char *domain)
{
	NTLM_ASSERT_ARG(ntlm);
	ENSURE_INITIALIZED(ntlm);

	free_hostname(ntlm);

	if (hostname && (ntlm->hostname = strdup(hostname)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return -1;
	}

	if (domain && (ntlm->hostdomain = strdup(domain)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return -1;
	}

	if (hostname && supports_unicode(ntlm) && !ntlm_unicode_utf8_to_16(
			&ntlm->hostname_utf16,
			&ntlm->hostname_utf16_len,
			ntlm,
			hostname,
			strlen(hostname)))
		return -1;

	return 0;
}

static void free_credentials(ntlm_client *ntlm)
{
	if (ntlm->password)
		ntlm_memzero(ntlm->password, strlen(ntlm->password));

	if (ntlm->password_utf16)
		ntlm_memzero(ntlm->password_utf16, ntlm->password_utf16_len);

	reset(ntlm->username);
	reset(ntlm->username_upper);
	reset(ntlm->userdomain);
	reset(ntlm->password);

	reset(ntlm->username_utf16);
	reset(ntlm->username_upper_utf16);
	reset(ntlm->userdomain_utf16);
	reset(ntlm->password_utf16);

	ntlm->username_utf16_len = 0;
	ntlm->username_upper_utf16_len = 0;
	ntlm->userdomain_utf16_len = 0;
	ntlm->password_utf16_len = 0;
}

int ntlm_client_set_credentials(
	ntlm_client *ntlm,
	const char *username,
	const char *domain,
	const char *password)
{
	NTLM_ASSERT_ARG(ntlm);
	ENSURE_INITIALIZED(ntlm);

	free_credentials(ntlm);

	if ((username && (ntlm->username = strdup(username)) == NULL) ||
		(domain && (ntlm->userdomain = strdup(domain)) == NULL) ||
		(password && (ntlm->password = strdup(password)) == NULL)) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return -1;
	}

	if (username && supports_unicode(ntlm)) {
		if ((ntlm->username_upper = strdup(username)) == NULL) {
			ntlm_client_set_errmsg(ntlm, "out of memory");
			return -1;
		}
		utf8upr(ntlm->username_upper);

		if (!ntlm_unicode_utf8_to_16(
				&ntlm->username_utf16,
				&ntlm->username_utf16_len,
				ntlm,
				ntlm->username,
				strlen(ntlm->username)))
			return -1;

		if (!ntlm_unicode_utf8_to_16(
				&ntlm->username_upper_utf16,
				&ntlm->username_upper_utf16_len,
				ntlm,
				ntlm->username_upper,
				strlen(ntlm->username_upper)))
			return -1;
	}

	if (domain && supports_unicode(ntlm) && !ntlm_unicode_utf8_to_16(
			&ntlm->userdomain_utf16,
			&ntlm->userdomain_utf16_len,
			ntlm,
			ntlm->userdomain,
			strlen(ntlm->userdomain)))
		return -1;

	return 0;
}

int ntlm_client_set_target(ntlm_client *ntlm, const char *target)
{
	NTLM_ASSERT_ARG(ntlm);
	ENSURE_INITIALIZED(ntlm);

	free(ntlm->target);
	free(ntlm->target_utf16);

	ntlm->target = NULL;
	ntlm->target_utf16 = NULL;

	if (target) {
		if ((ntlm->target = strdup(target)) == NULL) {
			ntlm_client_set_errmsg(ntlm, "out of memory");
			return -1;
		}

		if (supports_unicode(ntlm) && !ntlm_unicode_utf8_to_16(
				&ntlm->target_utf16,
				&ntlm->target_utf16_len,
				ntlm,
				ntlm->target,
				strlen(ntlm->target)))
			return -1;
	}

	return 0;
}

int ntlm_client_set_nonce(ntlm_client *ntlm, uint64_t nonce)
{
	NTLM_ASSERT_ARG(ntlm);

	ntlm->nonce = nonce;
	return 0;
}

int ntlm_client_set_timestamp(ntlm_client *ntlm, uint64_t timestamp)
{
	NTLM_ASSERT_ARG(ntlm);

	ntlm->timestamp = timestamp;
	return 0;
}

NTLM_INLINE(bool) write_buf(
	ntlm_client *ntlm,
	ntlm_buf *out,
	const unsigned char *buf,
	size_t len)
{
	if (!len)
		return true;

	if (out->len - out->pos < len) {
		ntlm_client_set_errmsg(ntlm, "out of buffer space");
		return false;
	}

	memcpy(&out->buf[out->pos], buf, len);
	out->pos += len;
	return true;
}

NTLM_INLINE(bool) write_byte(
	ntlm_client *ntlm,
	ntlm_buf *out,
	uint8_t value)
{
	if (out->len - out->pos < 1) {
		ntlm_client_set_errmsg(ntlm, "out of buffer space");
		return false;
	}

	out->buf[out->pos++] = value;
	return true;
}

NTLM_INLINE(bool) write_int16(
	ntlm_client *ntlm,
	ntlm_buf *out,
	uint16_t value)
{
	if (out->len - out->pos < 2) {
		ntlm_client_set_errmsg(ntlm, "out of buffer space");
		return false;
	}

	out->buf[out->pos++] = (value & 0x000000ff);
	out->buf[out->pos++] = (value & 0x0000ff00) >> 8;
	return true;
}

NTLM_INLINE(bool) write_int32(
	ntlm_client *ntlm,
	ntlm_buf *out,
	uint32_t value)
{
	if (out->len - out->pos < 2) {
		ntlm_client_set_errmsg(ntlm, "out of buffer space");
		return false;
	}

	out->buf[out->pos++] = (value & 0x000000ff);
	out->buf[out->pos++] = (value & 0x0000ff00) >> 8;
	out->buf[out->pos++] = (value & 0x00ff0000) >> 16;
	out->buf[out->pos++] = (value & 0xff000000) >> 24;
	return true;
}

NTLM_INLINE(bool) write_version(
	ntlm_client *ntlm,
	ntlm_buf *out,
	ntlm_version *version)
{
	return write_byte(ntlm, out, version->major) &&
		write_byte(ntlm, out, version->minor) &&
		write_int16(ntlm, out, version->build) &&
		write_int32(ntlm, out, version->reserved);
}

NTLM_INLINE(bool) write_bufinfo(
	ntlm_client *ntlm,
	ntlm_buf *out,
	size_t len,
	size_t offset)
{
	if (len > UINT16_MAX) {
		ntlm_client_set_errmsg(ntlm, "invalid string, too long");
		return false;
	}

	if (offset > UINT32_MAX) {
		ntlm_client_set_errmsg(ntlm, "invalid string, invalid offset");
		return false;
	}

	return write_int16(ntlm, out, (uint16_t)len) &&
		write_int16(ntlm, out, (uint16_t)len) &&
		write_int32(ntlm, out, (uint32_t)offset);
}

NTLM_INLINE(bool) read_buf(
	unsigned char *out,
	ntlm_client *ntlm,
	ntlm_buf *message,
	size_t len)
{
	if (message->len - message->pos < len) {
		ntlm_client_set_errmsg(ntlm, "truncated message");
		return false;
	}

	memcpy(out, &message->buf[message->pos], len);
	message->pos += len;

	return true;
}

NTLM_INLINE(bool) read_byte(
	uint8_t *out,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	if (message->len - message->pos < 1) {
		ntlm_client_set_errmsg(ntlm, "truncated message");
		return false;
	}

	*out = message->buf[message->pos++];
	return true;
}

NTLM_INLINE(bool) read_int16(
	uint16_t *out,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	if (message->len - message->pos < 2) {
		ntlm_client_set_errmsg(ntlm, "truncated message");
		return false;
	}

	*out =
		((message->buf[message->pos]   & 0xff)) |
		((message->buf[message->pos+1] & 0xff) << 8);

	message->pos += 2;
	return true;
}

NTLM_INLINE(bool) read_int32(
	uint32_t *out,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	if (message->len - message->pos < 4) {
		ntlm_client_set_errmsg(ntlm, "truncated message");
		return false;
	}

	*out =
		((message->buf[message->pos]   & 0xff)) |
		((message->buf[message->pos+1] & 0xff) << 8) |
		((message->buf[message->pos+2] & 0xff) << 16) |
		((message->buf[message->pos+3] & 0xff) << 24);

	message->pos += 4;
	return true;
}

NTLM_INLINE(bool) read_int64(
	uint64_t *out,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	if (message->len - message->pos < 8) {
		ntlm_client_set_errmsg(ntlm, "truncated message");
		return false;
	}

	*out =
		((uint64_t)(message->buf[message->pos]   & 0xff)) |
		((uint64_t)(message->buf[message->pos+1] & 0xff) << 8) |
		((uint64_t)(message->buf[message->pos+2] & 0xff) << 16) |
		((uint64_t)(message->buf[message->pos+3] & 0xff) << 24) |
		((uint64_t)(message->buf[message->pos+4] & 0xff) << 32) |
		((uint64_t)(message->buf[message->pos+5] & 0xff) << 40) |
		((uint64_t)(message->buf[message->pos+6] & 0xff) << 48) |
		((uint64_t)(message->buf[message->pos+7] & 0xff) << 56);

	message->pos += 8;
	return true;
}

NTLM_INLINE(bool) read_version(
	ntlm_version *out,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	return read_byte(&out->major, ntlm, message) &&
		read_byte(&out->minor, ntlm, message) &&
		read_int16(&out->build, ntlm, message) &&
		read_int32(&out->reserved, ntlm, message);
}

NTLM_INLINE(bool) read_bufinfo(
	uint16_t *out_len,
	uint32_t *out_offset,
	ntlm_client *ntlm,
	ntlm_buf *message)
{
	uint16_t allocated;

	return read_int16(out_len, ntlm, message) &&
		read_int16(&allocated, ntlm, message) &&
		read_int32(out_offset, ntlm, message);
}

NTLM_INLINE(bool) read_string_unicode(
	char **out,
	ntlm_client *ntlm,
	ntlm_buf *message,
	uint8_t string_len)
{
	size_t out_len;
	int ret = ntlm_unicode_utf16_to_8(out,
		&out_len,
		ntlm,
		(char *)&message->buf[message->pos],
		string_len);

	message->pos += string_len;

	return ret;
}

NTLM_INLINE(bool) read_string_ascii(
	char **out,
	ntlm_client *ntlm,
	ntlm_buf *message,
	uint8_t string_len)
{
	char *str;

	if ((str = malloc(string_len + 1)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return false;
	}

	memcpy(str, &message->buf[message->pos], string_len);
	str[string_len] = '\0';

	message->pos += string_len;

	*out = str;
	return true;
}

NTLM_INLINE(bool) read_string(
	char **out,
	ntlm_client *ntlm,
	ntlm_buf *message,
	uint8_t string_len,
	bool unicode)
{
	if (unicode)
		return read_string_unicode(out, ntlm, message, string_len);
	else
		return read_string_ascii(out, ntlm, message, string_len);
}

NTLM_INLINE(bool) read_target_info(
	char **server_out,
	char **domain_out,
	char **server_dns_out,
	char **domain_dns_out,
	ntlm_client *ntlm,
	ntlm_buf *message,
	bool unicode)
{
	uint16_t block_type, block_len;
	bool done = false;

	*server_out = NULL;
	*domain_out = NULL;
	*server_dns_out = NULL;
	*domain_dns_out = NULL;

	while (!done && (message->len - message->pos) >= 4) {
		if (!read_int16(&block_type, ntlm, message) ||
			!read_int16(&block_len, ntlm, message)) {
			ntlm_client_set_errmsg(ntlm, "truncated target info block");
			return false;
		}

		if (!block_type && block_len) {
			ntlm_client_set_errmsg(ntlm, "invalid target info block");
			return -1;
		}

		switch (block_type) {
		case NTLM_TARGET_INFO_DOMAIN:
			if (!read_string(domain_out, ntlm, message, block_len, unicode))
				return -1;
			break;
		case NTLM_TARGET_INFO_SERVER:
			if (!read_string(server_out, ntlm, message, block_len, unicode))
				return -1;
			break;
		case NTLM_TARGET_INFO_DOMAIN_DNS:
			if (!read_string(domain_dns_out, ntlm, message, block_len, unicode))
				return -1;
			break;
		case NTLM_TARGET_INFO_SERVER_DNS:
			if (!read_string(server_dns_out, ntlm, message, block_len, unicode))
				return -1;
			break;
		case NTLM_TARGET_INFO_END:
			done = true;
			break;
		default:
			ntlm_client_set_errmsg(ntlm, "unknown target info block type");
			return -1;
		}
	}

	if (message->len != message->pos) {
		ntlm_client_set_errmsg(ntlm,
			"invalid extra data in target info section");
		return false;
	}

	return true;
}

int ntlm_client_negotiate(
	const unsigned char **out,
	size_t *out_len,
	ntlm_client *ntlm)
{
	size_t hostname_len, domain_len;
	size_t domain_offset = 0;
	size_t hostname_offset = 0;
	uint32_t flags = 0;

	NTLM_ASSERT_ARG(out);
	NTLM_ASSERT_ARG(out_len);
	NTLM_ASSERT_ARG(ntlm);

	*out = NULL;
	*out_len = 0;

	if (ntlm->state != NTLM_STATE_NEGOTIATE) {
		ntlm_client_set_errmsg(ntlm, "ntlm handle in invalid state");
		return -1;
	}

	flags |= NTLM_NEGOTIATE_OEM;

	if (supports_unicode(ntlm))
		flags |= NTLM_NEGOTIATE_UNICODE;

	if (!(ntlm->flags & NTLM_CLIENT_DISABLE_NTLM2) ||
		(ntlm->flags & NTLM_CLIENT_ENABLE_NTLM))
		flags |= NTLM_NEGOTIATE_NTLM;

	if (!(ntlm->flags & NTLM_CLIENT_DISABLE_REQUEST_TARGET))
		flags |= NTLM_NEGOTIATE_REQUEST_TARGET;

	hostname_len = ntlm->hostname ? strlen(ntlm->hostname) : 0;
	domain_len = ntlm->hostdomain ? strlen(ntlm->hostdomain) : 0;

	/* Minimum header size */
	ntlm->negotiate.len = 16;

	/* Include space for security buffer descriptors */
	if (domain_len)
		increment_size(&ntlm->negotiate.len, 8);

	if (hostname_len)
		increment_size(&ntlm->negotiate.len, 8);

	if (ntlm->flags & NTLM_ENABLE_HOSTVERSION)
		increment_size(&ntlm->negotiate.len, 8);

	/* Location of security buffers */
	if (hostname_len) {
		flags |= NTLM_NEGOTIATE_WORKSTATION_SUPPLIED;
		hostname_offset = ntlm->negotiate.len;
		increment_size(&ntlm->negotiate.len, hostname_len);
	}

	if (domain_len) {
		flags |= NTLM_NEGOTIATE_DOMAIN_SUPPLIED;
		domain_offset = ntlm->negotiate.len;
		increment_size(&ntlm->negotiate.len, domain_len);
	}

	if (ntlm->negotiate.len == (size_t)-1) {
		ntlm_client_set_errmsg(ntlm, "message too large");
		return -1;
	}

	if ((ntlm->negotiate.buf = calloc(1, ntlm->negotiate.len)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return -1;
	}

	if (!write_buf(ntlm, &ntlm->negotiate,
			ntlm_client_signature, sizeof(ntlm_client_signature)) ||
		!write_int32(ntlm, &ntlm->negotiate, 1) ||
		!write_int32(ntlm, &ntlm->negotiate, flags))
		return -1;

	/* Domain information */
	if (domain_len > 0 &&
		!write_bufinfo(ntlm, &ntlm->negotiate, domain_len, domain_offset))
		return -1;

	/* Workstation information */
	if (hostname_len > 0 &&
		!write_bufinfo(ntlm, &ntlm->negotiate, hostname_len, hostname_offset))
		return -1;

	/* Version number */
	if (!!(ntlm->flags & NTLM_ENABLE_HOSTVERSION) &&
		!write_version(ntlm, &ntlm->negotiate, &ntlm->host_version))
		return -1;

	if (hostname_len > 0) {
		NTLM_ASSERT(ntlm, hostname_offset == ntlm->negotiate.pos);

		if (!write_buf(ntlm, &ntlm->negotiate,
			(const unsigned char *)ntlm->hostname, hostname_len))
			return -1;
	}

	if (domain_len > 0) {
		NTLM_ASSERT(ntlm, domain_offset == ntlm->negotiate.pos);

		if (!write_buf(ntlm, &ntlm->negotiate,
			(const unsigned char *)ntlm->hostdomain, domain_len))
			return -1;
	}

	NTLM_ASSERT(ntlm, ntlm->negotiate.pos == ntlm->negotiate.len);

	ntlm->state = NTLM_STATE_CHALLENGE;

	*out = ntlm->negotiate.buf;
	*out_len = ntlm->negotiate.len;

	return 0;
}

int ntlm_client_set_challenge(
	ntlm_client *ntlm,
	const unsigned char *challenge_msg,
	size_t challenge_msg_len)
{
	unsigned char signature[8];
	ntlm_buf challenge;
	uint32_t type_indicator, header_end;
	uint16_t name_len, info_len = 0;
	uint32_t name_offset, info_offset = 0;
	bool unicode, has_target_info = false;

	NTLM_ASSERT_ARG(ntlm);
	NTLM_ASSERT_ARG(challenge_msg || !challenge_msg_len);

	ENSURE_INITIALIZED(ntlm);

	if (ntlm->state != NTLM_STATE_NEGOTIATE &&
		ntlm->state != NTLM_STATE_CHALLENGE) {
		ntlm_client_set_errmsg(ntlm, "ntlm handle in invalid state");
		return -1;
	}

	challenge.buf = (unsigned char *)challenge_msg;
	challenge.len = challenge_msg_len;
	challenge.pos = 0;

	if (!read_buf(signature, ntlm, &challenge, 8) ||
		!read_int32(&type_indicator, ntlm, &challenge) ||
		!read_bufinfo(&name_len, &name_offset, ntlm, &challenge) ||
		!read_int32(&ntlm->challenge.flags, ntlm, &challenge) ||
		!read_int64(&ntlm->challenge.nonce, ntlm, &challenge))
		return -1;

	if (memcmp(signature,
			ntlm_client_signature, sizeof(ntlm_client_signature)) != 0) {
		ntlm_client_set_errmsg(ntlm, "invalid message signature");
		return -1;
	}

	if (type_indicator != 2) {
		ntlm_client_set_errmsg(ntlm, "invalid message indicator");
		return -1;
	}

	/*
	 * If there's additional space before the data section, that's the
	 * target information description section.
	 */
	header_end = challenge.len;

	if (name_offset && name_offset < header_end)
		header_end = name_offset;

	if ((header_end - challenge.pos) >= 16) {
		has_target_info = true;
	}

	if (!has_target_info &&
		(ntlm->challenge.flags & NTLM_NEGOTIATE_TARGET_INFO)) {
		ntlm_client_set_errmsg(ntlm,
			"truncated message; expected target info");
		return -1;
	}

	/*
	 * If there's a target info section then advanced over the reserved
	 * space and read the target information.
	 */
	if (has_target_info) {
		uint64_t reserved;

		if (!read_int64(&reserved, ntlm, &challenge)) {
			ntlm_client_set_errmsg(ntlm,
				"truncated message; expected reserved space");
			return -1;
		}

		if (reserved != 0) {
			ntlm_client_set_errmsg(ntlm,
				"invalid message; expected reserved space to be empty");
			return -1;
		}

		if (!read_bufinfo(&info_len, &info_offset, ntlm, &challenge)) {
			ntlm_client_set_errmsg(ntlm,
				"truncated message; expected target info");
			return -1;
		}
	}

	unicode = !!(ntlm->challenge.flags & NTLM_NEGOTIATE_UNICODE);

	/*
	 * If there's still additional space before the data section,
	 * that's the server's version information.
	 */
	if (info_offset && info_offset < header_end)
		header_end = info_offset;

	if (ntlm->challenge.flags & NTLM_NEGOTIATE_VERSION) {
		if ((header_end - challenge.pos) != sizeof(ntlm_version) ||
			!read_version(&ntlm->challenge.target_version,
				ntlm, &challenge)) {
			ntlm_client_set_errmsg(ntlm,
				"truncated message; expected version");
			return -1;
		}
	}

	/* validate data section */
	if ((name_offset && name_offset < challenge.pos) ||
		challenge.len < name_len ||
		(challenge.len - name_len) < name_offset) {
		ntlm_client_set_errmsg(ntlm,
			"invalid message; invalid target name buffer");
		return -1;
	}
	if ((info_offset && info_offset < challenge.pos) ||
		challenge.len < info_len ||
		(challenge.len - info_len) < info_offset) {
		ntlm_client_set_errmsg(ntlm,
			"invalid message; invalid target info buffer");
		return -1;
	}

	/* advance to the data section */
	if (name_len && name_offset) {
		challenge.pos = name_offset;

		if (!read_string(&ntlm->challenge.target,
			ntlm, &challenge, name_len, unicode)) {
			ntlm_client_set_errmsg(ntlm,
				"truncated message; truncated target name");
			return -1;
		}
	}

	if (info_len && info_offset) {
		ntlm_buf info_buf;

		challenge.pos = info_offset;

		/* create a copy of the target info; we need the literal data */
		if ((ntlm->challenge.target_info = malloc(info_len)) == NULL) {
			ntlm_client_set_errmsg(ntlm, "out of memory");
			return -1;
		}

		if (!read_buf(ntlm->challenge.target_info,
				ntlm, &challenge, info_len)) {
			ntlm_client_set_errmsg(ntlm,
				"truncated message; truncated target info");
			return -1;
		}

		info_buf.buf = ntlm->challenge.target_info;
		info_buf.pos = 0;
		info_buf.len = info_len;

		/* then set up the target info and parse it */
		if (!read_target_info(&ntlm->challenge.target_server,
				&ntlm->challenge.target_domain,
				&ntlm->challenge.target_server_dns,
				&ntlm->challenge.target_domain_dns,
				ntlm, &info_buf, unicode))
			return -1;

		ntlm->challenge.target_info_len = info_len;
	}

	ntlm->state = NTLM_STATE_RESPONSE;

	return 0;
}

uint64_t ntlm_client_challenge_nonce(ntlm_client *ntlm)
{
	return ntlm->challenge.nonce;
}

const char *ntlm_client_target(ntlm_client *ntlm)
{
	return ntlm->challenge.target;
}

const char *ntlm_client_target_server(ntlm_client *ntlm)
{
	return ntlm->challenge.target_server;
}

const char *ntlm_client_target_domain(ntlm_client *ntlm)
{
	return ntlm->challenge.target_domain;
}

const char *ntlm_client_target_server_dns(ntlm_client *ntlm)
{
	return ntlm->challenge.target_server_dns;
}

const char *ntlm_client_target_domain_dns(ntlm_client *ntlm)
{
	return ntlm->challenge.target_domain_dns;
}

#define EVEN_PARITY(a) \
	(!!((a) & INT64_C(0x01)) ^ !!((a) & INT64_C(0x02)) ^ \
	 !!((a) & INT64_C(0x04)) ^ !!((a) & INT64_C(0x08)) ^ \
	 !!((a) & INT64_C(0x10)) ^ !!((a) & INT64_C(0x20)) ^ \
	 !!((a) & INT64_C(0x40)) ^ !!((a) & INT64_C(0x80)))

static void generate_odd_parity(ntlm_des_block *block)
{
	size_t i;

	for (i = 0; i < sizeof(ntlm_des_block); i++)
		(*block)[i] |= (1 ^ EVEN_PARITY((*block)[i]));
}

static void des_key_from_password(
	ntlm_des_block *out,
	const unsigned char *plaintext,
	size_t plaintext_len)
{
	size_t i;

	plaintext_len = MIN(plaintext_len, 7);

	memset(*out, 0, sizeof(ntlm_des_block));

	for (i = 0; i < plaintext_len; i++) {
		size_t j = (7 - i);
		uint8_t mask = (0xff >> j);

		(*out)[i]   |= ((plaintext[i] & (0xff - mask)) >> i);
		(*out)[i+1] |= ((plaintext[i] & mask) << j);
	}

	generate_odd_parity(out);
}

NTLM_INLINE(bool) generate_lm_hash(
	ntlm_des_block out[2],
	ntlm_client *ntlm,
	const char *password)
{
	/* LM encrypts this known plaintext using the password as a key */
	ntlm_des_block plaintext = NTLM_LM_PLAINTEXT;
	ntlm_des_block keystr1, keystr2;
	size_t keystr1_len, keystr2_len;
	ntlm_des_block key1, key2;
	size_t password_len, i;

	/* Copy the first 14 characters of the password, uppercased */
	memset(&keystr1, 0, sizeof(keystr1));
	memset(&keystr2, 0, sizeof(keystr2));

	password_len = password ? strlen(password) : 0;

	/* Split the password into two 7 byte chunks */
	keystr1_len = MIN(7, password_len);
	keystr2_len = (password_len > 7) ? MIN(14, password_len) - 7 : 0;

	for (i = 0; i < keystr1_len; i++)
		keystr1[i] = (unsigned char)toupper((unsigned char)password[i]);
	for (i = 0; i < keystr2_len; i++)
		keystr2[i] = (unsigned char)toupper((unsigned char)password[i+7]);

	/* DES encrypt the LM constant using the password as the key */
	des_key_from_password(&key1, keystr1, keystr1_len);
	des_key_from_password(&key2, keystr2, keystr2_len);

	return ntlm_des_encrypt(&out[0], ntlm, &plaintext, &key1) &&
		ntlm_des_encrypt(&out[1], ntlm, &plaintext, &key2);
}

static void des_keys_from_lm_hash(ntlm_des_block out[3], ntlm_des_block lm_hash[2])
{
	ntlm_des_block split[3];

	memcpy(&split[0][0], &lm_hash[0][0], 7);

	memcpy(&split[1][0], &lm_hash[0][7], 1);
	memcpy(&split[1][1], &lm_hash[1][0], 6);

	memcpy(&split[2][0], &lm_hash[1][6], 2);

	des_key_from_password(&out[0], split[0], 7);
	des_key_from_password(&out[1], split[1], 7);
	des_key_from_password(&out[2], split[2], 2);
}

static bool generate_lm_response(ntlm_client *ntlm)
{
	ntlm_des_block lm_hash[2], key[3], lm_response[3];
	ntlm_des_block *challenge = (ntlm_des_block *)&ntlm->challenge.nonce;

	/* Generate the LM hash from the password */
	if (!generate_lm_hash(lm_hash, ntlm, ntlm->password))
		return false;

	/* Convert that LM hash to three DES keys */
	des_keys_from_lm_hash(key, lm_hash);

	/* Finally, encrypt the challenge with each of these keys */
	if (!ntlm_des_encrypt(&lm_response[0], ntlm, challenge, &key[0]) ||
		!ntlm_des_encrypt(&lm_response[1], ntlm, challenge, &key[1]) ||
		!ntlm_des_encrypt(&lm_response[2], ntlm, challenge, &key[2]))
		return false;

	memcpy(&ntlm->lm_response[0], lm_response[0], 8);
	memcpy(&ntlm->lm_response[8], lm_response[1], 8);
	memcpy(&ntlm->lm_response[16], lm_response[2], 8);

	ntlm->lm_response_len = sizeof(ntlm->lm_response);

	return true;
}

static bool generate_ntlm_hash(
	unsigned char out[NTLM_NTLM_HASH_LEN], ntlm_client *ntlm)
{
	/* Generate the LM hash from the (Unicode) password */
	if (ntlm->password && !ntlm_unicode_utf8_to_16(
			&ntlm->password_utf16,
			&ntlm->password_utf16_len,
			ntlm,
			ntlm->password,
			strlen(ntlm->password)))
		return false;

	return ntlm_md4_digest(out,
		ntlm,
		(const unsigned char *)ntlm->password_utf16,
		ntlm->password_utf16_len);
}

static bool generate_ntlm_response(ntlm_client *ntlm)
{
	unsigned char ntlm_hash[NTLM_NTLM_HASH_LEN] = {0};
	ntlm_des_block key[3], ntlm_response[3];
	ntlm_des_block *challenge =
		(ntlm_des_block *)&ntlm->challenge.nonce;

	if (!generate_ntlm_hash(ntlm_hash, ntlm))
		return false;

	/* Convert that LM hash to three DES keys */
	des_key_from_password(&key[0], &ntlm_hash[0], 7);
	des_key_from_password(&key[1], &ntlm_hash[7], 7);
	des_key_from_password(&key[2], &ntlm_hash[14], 2);

	/* Finally, encrypt the challenge with each of these keys */
	if (!ntlm_des_encrypt(&ntlm_response[0], ntlm, challenge, &key[0]) ||
		!ntlm_des_encrypt(&ntlm_response[1], ntlm, challenge, &key[1]) ||
		!ntlm_des_encrypt(&ntlm_response[2], ntlm, challenge, &key[2]))
		return false;

	memcpy(&ntlm->ntlm_response[0], ntlm_response[0], 8);
	memcpy(&ntlm->ntlm_response[8], ntlm_response[1], 8);
	memcpy(&ntlm->ntlm_response[16], ntlm_response[2], 8);

	ntlm->ntlm_response_len = sizeof(ntlm->ntlm_response);
	return true;
}

static bool generate_ntlm2_hash(
	unsigned char out[NTLM_NTLM2_HASH_LEN], ntlm_client *ntlm)
{
	unsigned char ntlm_hash[NTLM_NTLM_HASH_LEN] = {0};
	const unsigned char *username = NULL, *target = NULL;
	size_t username_len = 0, target_len = 0, out_len = NTLM_NTLM2_HASH_LEN;

	if (!generate_ntlm_hash(ntlm_hash, ntlm))
		return false;

	if (ntlm->username_upper_utf16) {
		username = (const unsigned char *)ntlm->username_upper_utf16;
		username_len = ntlm->username_upper_utf16_len;
	}

	if (ntlm->target_utf16) {
		target = (const unsigned char *)ntlm->target_utf16;
		target_len = ntlm->target_utf16_len;
	}

	if (!ntlm_hmac_md5_init(ntlm, ntlm_hash, sizeof(ntlm_hash)) ||
		!ntlm_hmac_md5_update(ntlm, username, username_len) ||
		!ntlm_hmac_md5_update(ntlm, target, target_len) ||
		!ntlm_hmac_md5_final(out, &out_len, ntlm)) {
		ntlm_client_set_errmsg(ntlm, "failed to create HMAC-MD5");
		return false;
	}

	NTLM_ASSERT(ntlm, out_len == NTLM_NTLM2_HASH_LEN);
	return true;
}

static bool generate_ntlm2_challengehash(
	unsigned char out[16],
	ntlm_client *ntlm,
	unsigned char ntlm2_hash[NTLM_NTLM2_HASH_LEN],
	const unsigned char *blob,
	size_t blob_len)
{
	size_t out_len = 16;

	if (!ntlm_hmac_md5_init(ntlm, ntlm2_hash, NTLM_NTLM2_HASH_LEN) ||
		!ntlm_hmac_md5_update(ntlm, (const unsigned char *)&ntlm->challenge.nonce, 8) ||
		!ntlm_hmac_md5_update(ntlm, blob, blob_len) ||
		!ntlm_hmac_md5_final(out, &out_len, ntlm)) {
		ntlm_client_set_errmsg(ntlm, "failed to create HMAC-MD5");
		return false;
	}

	NTLM_ASSERT(ntlm, out_len == 16);
	return true;
}

static bool generate_lm2_response(ntlm_client *ntlm,
	unsigned char ntlm2_hash[NTLM_NTLM2_HASH_LEN])
{
	unsigned char lm2_challengehash[16] = {0};
	size_t lm2_len = 16;
	uint64_t local_nonce;

	local_nonce = ntlm_htonll(ntlm->nonce);

	if (!ntlm_hmac_md5_init(ntlm, ntlm2_hash, NTLM_NTLM2_HASH_LEN) ||
		!ntlm_hmac_md5_update(ntlm, (const unsigned char *)&ntlm->challenge.nonce, 8) ||
		!ntlm_hmac_md5_update(ntlm, (const unsigned char *)&local_nonce, 8) ||
		!ntlm_hmac_md5_final(lm2_challengehash, &lm2_len, ntlm)) {
		ntlm_client_set_errmsg(ntlm, "failed to create HMAC-MD5");
		return false;
	}

	NTLM_ASSERT(ntlm, lm2_len == 16);

	memcpy(&ntlm->lm_response[0], lm2_challengehash, 16);
	memcpy(&ntlm->lm_response[16], &local_nonce, 8);

	ntlm->lm_response_len = 24;
	return true;
}

static bool generate_timestamp(ntlm_client *ntlm)
{
	if (!ntlm->timestamp)
		ntlm->timestamp = (time(NULL) + 11644473600) * 10000000;

	return true;
}

static bool generate_nonce(ntlm_client *ntlm)
{
	unsigned char buf[8];

	if (ntlm->nonce)
		return true;

	if (!ntlm_random_bytes(buf, ntlm, 8))
		return false;

	memcpy(&ntlm->nonce, buf, sizeof(uint64_t));
	return true;
}

static bool generate_ntlm2_response(ntlm_client *ntlm)
{
	size_t blob_len, ntlm2_response_len;
	uint32_t signature;
	uint64_t timestamp, nonce;
	unsigned char ntlm2_hash[NTLM_NTLM2_HASH_LEN];
	unsigned char challengehash[16] = {0};
	unsigned char *blob;

	if (!generate_timestamp(ntlm) ||
		!generate_nonce(ntlm) ||
		!generate_ntlm2_hash(ntlm2_hash, ntlm))
		return false;

	blob_len = ntlm->challenge.target_info_len + 32;
	ntlm2_response_len = blob_len + 16;

	if ((ntlm->ntlm2_response = malloc(ntlm2_response_len)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return false;
	}

	/* position the blob in the response; we'll use it then return it */
	blob = ntlm->ntlm2_response + 16;

	/* the blob's integer values are in network byte order */
	signature = htonl(0x01010000);
	timestamp = ntlm_htonll(ntlm->timestamp);
	nonce = ntlm_htonll(ntlm->nonce);

	/* construct the blob */
	memcpy(&blob[0], &signature, 4);
	memset(&blob[4], 0, 4);
	memcpy(&blob[8], &timestamp, 8);
	memcpy(&blob[16], &nonce, 8);
	memset(&blob[24], 0, 4);
	memcpy(&blob[28], ntlm->challenge.target_info, ntlm->challenge.target_info_len);
	memset(&blob[28 + ntlm->challenge.target_info_len], 0, 4);

	if (!generate_ntlm2_challengehash(challengehash, ntlm, ntlm2_hash, blob, blob_len))
		return false;

	memcpy(ntlm->ntlm2_response, challengehash, 16);
	ntlm->ntlm2_response_len = ntlm2_response_len;

	if (!generate_lm2_response(ntlm, ntlm2_hash))
		return false;

	return true;
}

int ntlm_client_response(
	const unsigned char **out,
	size_t *out_len,
	ntlm_client *ntlm)
{
	unsigned char *domain, *username, *hostname, *ntlm_rep, *session;
	size_t lm_rep_len, lm_rep_offset, ntlm_rep_len, ntlm_rep_offset,
		domain_len, domain_offset, username_len, username_offset,
		hostname_len, hostname_offset, session_len, session_offset;
	uint32_t flags = 0;
	bool unicode;

	NTLM_ASSERT_ARG(out);
	NTLM_ASSERT_ARG(out_len);
	NTLM_ASSERT_ARG(ntlm);

	ENSURE_INITIALIZED(ntlm);

	*out = NULL;
	*out_len = 0;

	if (ntlm->state != NTLM_STATE_RESPONSE) {
		ntlm_client_set_errmsg(ntlm, "ntlm handle in invalid state");
		return -1;
	}

	/*
	 * Minimum message size is 64 bytes:
	 *   8 byte signature,
	 *   4 byte message indicator,
	 * 6x8 byte security buffers
	 *   4 byte flags
	 */
	ntlm->response.len = 64;

	unicode = supports_unicode(ntlm) &&
		(ntlm->challenge.flags & NTLM_NEGOTIATE_UNICODE);

	if (unicode)
		flags |= NTLM_NEGOTIATE_UNICODE;
	else
		flags |= NTLM_NEGOTIATE_OEM;

	if (unicode) {
		domain = (unsigned char *)ntlm->userdomain_utf16;
		domain_len = ntlm->userdomain_utf16_len;

		username = (unsigned char *)ntlm->username_utf16;
		username_len = ntlm->username_utf16_len;

		hostname = (unsigned char *)ntlm->hostname_utf16;
		hostname_len = ntlm->hostname_utf16_len;
	} else {
		domain = (unsigned char *)ntlm->userdomain;
		domain_len = ntlm->userdomain ? strlen(ntlm->userdomain) : 0;

		username = (unsigned char *)ntlm->username;
		username_len = ntlm->username ? strlen(ntlm->username) : 0;

		hostname = (unsigned char *)ntlm->hostname;
		hostname_len = ntlm->hostname ? strlen(ntlm->hostname) : 0;
	}

	/* Negotiate our requested authentication type with the server's */
	if (!(ntlm->flags & NTLM_CLIENT_DISABLE_NTLM2) &&
		(ntlm->challenge.flags & NTLM_NEGOTIATE_NTLM)) {
		flags |= NTLM_NEGOTIATE_NTLM;

		if (!generate_ntlm2_response(ntlm))
			return -1;
	} else if ((ntlm->flags & NTLM_CLIENT_ENABLE_NTLM) &&
		(ntlm->challenge.flags & NTLM_NEGOTIATE_NTLM)) {
		flags |= NTLM_NEGOTIATE_NTLM;

		if (!generate_ntlm_response(ntlm) ||
			!generate_lm_response(ntlm))
			return -1;
	} else if (ntlm->flags & NTLM_CLIENT_ENABLE_LM) {
		if (!generate_lm_response(ntlm))
			return -1;
	} else {
		ntlm_client_set_errmsg(ntlm,
			"no encryption options could be negotiated");
		return -1;
	}

	domain_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, domain_len);

	username_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, username_len);

	hostname_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, hostname_len);

	lm_rep_len = ntlm->lm_response_len;
	lm_rep_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, lm_rep_len);

	ntlm_rep = ntlm->ntlm2_response_len ?
		ntlm->ntlm2_response : ntlm->ntlm_response;
	ntlm_rep_len = ntlm->ntlm2_response_len ?
		ntlm->ntlm2_response_len : ntlm->ntlm_response_len;
	ntlm_rep_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, ntlm_rep_len);

	session = NULL;
	session_len = 0;
	session_offset = ntlm->response.len;
	increment_size(&ntlm->response.len, session_len);

	if (ntlm->response.len == (size_t)-1) {
		ntlm_client_set_errmsg(ntlm, "message too large");
		return -1;
	}

	if ((ntlm->response.buf = calloc(1, ntlm->response.len)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return -1;
	}

	if (!write_buf(ntlm, &ntlm->response,
			ntlm_client_signature, sizeof(ntlm_client_signature)) ||
		!write_int32(ntlm, &ntlm->response, 3) ||
		!write_bufinfo(ntlm, &ntlm->response, lm_rep_len, lm_rep_offset) ||
		!write_bufinfo(ntlm, &ntlm->response, ntlm_rep_len, ntlm_rep_offset) ||
		!write_bufinfo(ntlm, &ntlm->response, domain_len, domain_offset) ||
		!write_bufinfo(ntlm, &ntlm->response, username_len, username_offset) ||
		!write_bufinfo(ntlm, &ntlm->response, hostname_len, hostname_offset) ||
		!write_bufinfo(ntlm, &ntlm->response, session_len, session_offset) ||
		!write_int32(ntlm, &ntlm->response, flags) ||
		!write_buf(ntlm, &ntlm->response, domain, domain_len) ||
		!write_buf(ntlm, &ntlm->response, username, username_len) ||
		!write_buf(ntlm, &ntlm->response, hostname, hostname_len) ||
		!write_buf(ntlm, &ntlm->response, ntlm->lm_response, lm_rep_len) ||
		!write_buf(ntlm, &ntlm->response, ntlm_rep, ntlm_rep_len) ||
		!write_buf(ntlm, &ntlm->response, session, session_len))
		return -1;

	NTLM_ASSERT(ntlm, ntlm->response.pos == ntlm->response.len);

	ntlm->state = NTLM_STATE_COMPLETE;

	*out = ntlm->response.buf;
	*out_len = ntlm->response.len;

	return 0;
}

void ntlm_client_reset(ntlm_client *ntlm)
{
	if (!ntlm)
		return;

	ntlm->state = NTLM_STATE_NEGOTIATE;

	free_hostname(ntlm);

	memset(&ntlm->host_version, 0, sizeof(ntlm_version));

	reset(ntlm->target);
	reset(ntlm->target_utf16);
	ntlm->target_utf16_len = 0;

	free_credentials(ntlm);

	ntlm->nonce = 0;
	ntlm->timestamp = 0;

	memset(ntlm->lm_response, 0, NTLM_LM_RESPONSE_LEN);
	ntlm->lm_response_len = 0;

	memset(ntlm->ntlm_response, 0, NTLM_NTLM_RESPONSE_LEN);
	ntlm->ntlm_response_len = 0;

	reset(ntlm->ntlm2_response);
	ntlm->ntlm2_response_len = 0;

	reset(ntlm->negotiate.buf);
	ntlm->negotiate.pos = 0;
	ntlm->negotiate.len = 0;

	reset(ntlm->response.buf);
	ntlm->response.pos = 0;
	ntlm->response.len = 0;

	free(ntlm->challenge.target_info);
	free(ntlm->challenge.target);
	free(ntlm->challenge.target_domain);
	free(ntlm->challenge.target_domain_dns);
	free(ntlm->challenge.target_server);
	free(ntlm->challenge.target_server_dns);
	memset(&ntlm->challenge, 0, sizeof(ntlm_challenge));
}

void ntlm_client_free(ntlm_client *ntlm)
{
	if (!ntlm)
		return;

	ntlm_crypt_shutdown(ntlm);
	ntlm_unicode_shutdown(ntlm);

	ntlm_client_reset(ntlm);

	free(ntlm);
}
