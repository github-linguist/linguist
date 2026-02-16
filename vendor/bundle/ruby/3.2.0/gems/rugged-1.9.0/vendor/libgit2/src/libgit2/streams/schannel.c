/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "streams/schannel.h"

#ifdef GIT_SCHANNEL

#define SECURITY_WIN32

#include <security.h>
#include <schannel.h>
#include <sspi.h>

#include "stream.h"
#include "streams/socket.h"

#ifndef SP_PROT_TLS1_2_CLIENT
# define SP_PROT_TLS1_2_CLIENT 2048
#endif

#ifndef SP_PROT_TLS1_3_CLIENT
# define SP_PROT_TLS1_3_CLIENT 8192
#endif

#ifndef SECBUFFER_ALERT
# define SECBUFFER_ALERT 17
#endif

#define READ_BLOCKSIZE (16 * 1024)

typedef enum {
	STATE_NONE = 0,
	STATE_CRED = 1,
	STATE_CONTEXT = 2,
	STATE_CERTIFICATE = 3
} schannel_state;

typedef struct {
	git_stream parent;
	git_stream *io;
	int owned;
	bool connected;
	wchar_t *host_w;

	schannel_state state;

	CredHandle cred;
	CtxtHandle context;
	SecPkgContext_StreamSizes stream_sizes;

	CERT_CONTEXT *certificate;
	const CERT_CHAIN_CONTEXT *cert_chain;
	git_cert_x509 x509;

	git_str plaintext_in;
	git_str ciphertext_in;
} schannel_stream;

static int connect_context(schannel_stream *st)
{
	SCHANNEL_CRED cred = { 0 };
	SECURITY_STATUS status = SEC_E_INTERNAL_ERROR;
	DWORD context_flags;
	static size_t MAX_RETRIES = 1024;
	size_t retries;
	ssize_t read_len;
	int error = 0;

	if (st->owned && (error = git_stream_connect(st->io)) < 0)
		return error;

	cred.dwVersion = SCHANNEL_CRED_VERSION;
	cred.dwFlags = SCH_CRED_IGNORE_NO_REVOCATION_CHECK |
	               SCH_CRED_IGNORE_REVOCATION_OFFLINE |
	               SCH_CRED_MANUAL_CRED_VALIDATION |
	               SCH_CRED_NO_DEFAULT_CREDS |
	               SCH_CRED_NO_SERVERNAME_CHECK;
	cred.grbitEnabledProtocols = SP_PROT_TLS1_2_CLIENT |
	                             SP_PROT_TLS1_3_CLIENT;

	if (AcquireCredentialsHandleW(NULL, SCHANNEL_NAME_W,
			SECPKG_CRED_OUTBOUND, NULL, &cred, NULL,
			NULL, &st->cred, NULL) != SEC_E_OK) {
		git_error_set(GIT_ERROR_OS, "could not acquire credentials handle");
		return -1;
	}

	st->state = STATE_CRED;

	context_flags = ISC_REQ_ALLOCATE_MEMORY |
	                ISC_REQ_CONFIDENTIALITY |
	                ISC_REQ_REPLAY_DETECT |
	                ISC_REQ_SEQUENCE_DETECT |
	                ISC_REQ_STREAM;

	for (retries = 0; retries < MAX_RETRIES; retries++) {
		SecBuffer input_buf[] = {
			{ (unsigned long)st->ciphertext_in.size,
			  SECBUFFER_TOKEN,
			  st->ciphertext_in.size ? st->ciphertext_in.ptr : NULL },
			{ 0, SECBUFFER_EMPTY, NULL }
		};
		SecBuffer output_buf[] = { { 0, SECBUFFER_TOKEN, NULL },
		                           { 0, SECBUFFER_ALERT, NULL } };

		SecBufferDesc input_buf_desc = { SECBUFFER_VERSION, 2, input_buf };
		SecBufferDesc output_buf_desc = { SECBUFFER_VERSION, 2, output_buf };

		status = InitializeSecurityContextW(&st->cred,
			retries ? &st->context : NULL, st->host_w,
			context_flags, 0, 0, retries ? &input_buf_desc : NULL, 0,
			retries ? NULL : &st->context, &output_buf_desc,
			&context_flags, NULL);

		if (status == SEC_E_OK || status == SEC_I_CONTINUE_NEEDED) {
			st->state = STATE_CONTEXT;

			if (output_buf[0].cbBuffer > 0) {
				error = git_stream__write_full(st->io,
					output_buf[0].pvBuffer,
					output_buf[0].cbBuffer, 0);

				FreeContextBuffer(output_buf[0].pvBuffer);
			}

			/* handle any leftover, unprocessed data */
			if (input_buf[1].BufferType == SECBUFFER_EXTRA) {
				GIT_ASSERT(st->ciphertext_in.size > input_buf[1].cbBuffer);

				git_str_consume_bytes(&st->ciphertext_in,
					st->ciphertext_in.size - input_buf[1].cbBuffer);
			} else {
				git_str_clear(&st->ciphertext_in);
			}

			if (error < 0 || status == SEC_E_OK)
				break;
			} else if (status == SEC_E_INCOMPLETE_MESSAGE) {
				/* we need additional data from the client; */
				if (git_str_grow_by(&st->ciphertext_in, READ_BLOCKSIZE) < 0) {
					error = -1;
				break;
			}

			if ((read_len = git_stream_read(st->io,
					st->ciphertext_in.ptr + st->ciphertext_in.size,
					(st->ciphertext_in.asize - st->ciphertext_in.size))) < 0) {
				error = -1;
				break;
			}

			GIT_ASSERT((size_t)read_len <=
				st->ciphertext_in.asize - st->ciphertext_in.size);
			st->ciphertext_in.size += read_len;
		} else {
			git_error_set(GIT_ERROR_OS,
				"could not initialize security context");
			error = -1;
			break;
		}

		GIT_ASSERT(st->ciphertext_in.size < ULONG_MAX);
	}

	if (retries == MAX_RETRIES) {
		git_error_set(GIT_ERROR_SSL,
			"could not initialize security context: too many retries");
		error = -1;
	}

	if (!error) {
		if (QueryContextAttributesW(&st->context,
				SECPKG_ATTR_STREAM_SIZES,
				&st->stream_sizes) != SEC_E_OK) {
			git_error_set(GIT_ERROR_SSL,
				"could not query stream sizes");
			error = -1;
		}
	}

	return error;
}

static int set_certificate_lookup_error(DWORD status)
{
	switch (status) {
	case CERT_TRUST_IS_NOT_TIME_VALID:
		git_error_set(GIT_ERROR_SSL,
			"certificate is expired or not yet valid");
		break;
	case CERT_TRUST_IS_REVOKED:
		git_error_set(GIT_ERROR_SSL, "certificate is revoked");
		break;
	case CERT_TRUST_IS_NOT_SIGNATURE_VALID:
	case CERT_TRUST_IS_NOT_VALID_FOR_USAGE:
	case CERT_TRUST_INVALID_EXTENSION:
	case CERT_TRUST_INVALID_POLICY_CONSTRAINTS:
	case CERT_TRUST_INVALID_BASIC_CONSTRAINTS:
	case CERT_TRUST_INVALID_NAME_CONSTRAINTS:
	case CERT_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT:
	case CERT_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT:
	case CERT_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT:
	case CERT_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT:
	case CERT_TRUST_NO_ISSUANCE_CHAIN_POLICY:
	case CERT_TRUST_HAS_NOT_SUPPORTED_CRITICAL_EXT:
		git_error_set(GIT_ERROR_SSL, "certificate is not valid");
		break;
	case CERT_TRUST_IS_UNTRUSTED_ROOT:
	case CERT_TRUST_IS_CYCLIC:
	case CERT_TRUST_IS_EXPLICIT_DISTRUST:
		git_error_set(GIT_ERROR_SSL, "certificate is not trusted");
		break;
	case CERT_TRUST_REVOCATION_STATUS_UNKNOWN:
		git_error_set(GIT_ERROR_SSL,
			"certificate revocation status could not be verified");
		break;
	case CERT_TRUST_IS_OFFLINE_REVOCATION:
		git_error_set(GIT_ERROR_SSL,
			"certificate revocation is offline or stale");
		break;
	case CERT_TRUST_HAS_WEAK_SIGNATURE:
		git_error_set(GIT_ERROR_SSL, "certificate has a weak signature");
		break;
	default:
		git_error_set(GIT_ERROR_SSL,
			"unknown certificate lookup failure: %d", status);
		return -1;
	}

	return GIT_ECERTIFICATE;
}

static int set_certificate_validation_error(DWORD status)
{
	switch (status) {
	case TRUST_E_CERT_SIGNATURE:
		git_error_set(GIT_ERROR_SSL,
			"the certificate cannot be verified");
		break;
	case CRYPT_E_REVOKED:
		git_error_set(GIT_ERROR_SSL,
			"the certificate or signature has been revoked");
		break;
	case CERT_E_UNTRUSTEDROOT:
		git_error_set(GIT_ERROR_SSL,
			"the certificate root is not trusted");
		break;
	case CERT_E_UNTRUSTEDTESTROOT:
		git_error_set(GIT_ERROR_SSL,
			"the certificate root is a test certificate");
		break;
	case CERT_E_CHAINING:
		git_error_set(GIT_ERROR_SSL,
			"the certificate chain is invalid");
		break;
	case CERT_E_WRONG_USAGE:
	case CERT_E_PURPOSE:
		git_error_set(GIT_ERROR_SSL,
			"the certificate is not valid for this usage");
		break;
	case CERT_E_EXPIRED:
		git_error_set(GIT_ERROR_SSL,
			"certificate is expired or not yet valid");
		break;
	case CERT_E_INVALID_NAME:
	case CERT_E_CN_NO_MATCH:
		git_error_set(GIT_ERROR_SSL,
			"certificate is not valid for this hostname");
		break;
	case CERT_E_INVALID_POLICY:
	case TRUST_E_BASIC_CONSTRAINTS:
	case CERT_E_CRITICAL:
	case CERT_E_VALIDITYPERIODNESTING:
		git_error_set(GIT_ERROR_SSL, "certificate is not valid");
		break;
	case CRYPT_E_NO_REVOCATION_CHECK:
		git_error_set(GIT_ERROR_SSL,
			"certificate revocation status could not be verified");
		break;
	case CRYPT_E_REVOCATION_OFFLINE:
		git_error_set(GIT_ERROR_SSL,
			"certificate revocation is offline or stale");
		break;
	case CERT_E_ROLE:
		git_error_set(GIT_ERROR_SSL, "certificate authority is not valid");
		break;
	default:
		git_error_set(GIT_ERROR_SSL,
			"unknown certificate policy checking failure: %d",
			status);
		return -1;
	}

	return GIT_ECERTIFICATE;
}

static int check_certificate(schannel_stream* st)
{
	CERT_CHAIN_PARA cert_chain_parameters;
	SSL_EXTRA_CERT_CHAIN_POLICY_PARA ssl_policy_parameters;
	CERT_CHAIN_POLICY_PARA cert_policy_parameters =
		{ sizeof(CERT_CHAIN_POLICY_PARA), 0, &ssl_policy_parameters };
	CERT_CHAIN_POLICY_STATUS cert_policy_status;

	memset(&cert_chain_parameters, 0, sizeof(CERT_CHAIN_PARA));
	cert_chain_parameters.cbSize = sizeof(CERT_CHAIN_PARA);

	if (QueryContextAttributesW(&st->context,
			SECPKG_ATTR_REMOTE_CERT_CONTEXT,
			&st->certificate) != SEC_E_OK) {
		git_error_set(GIT_ERROR_OS,
			"could not query remote certificate context");
		return -1;
	}

	/* TODO: do we really want to do revokcation checking ? */
	if (!CertGetCertificateChain(NULL, st->certificate, NULL,
			st->certificate->hCertStore, &cert_chain_parameters,
			CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT,
			NULL, &st->cert_chain)) {
		git_error_set(GIT_ERROR_OS, "could not query remote certificate chain");
		CertFreeCertificateContext(st->certificate);
		return -1;
	}

	st->state = STATE_CERTIFICATE;

	/* Set up the x509 certificate data for future callbacks */

	st->x509.parent.cert_type = GIT_CERT_X509;
	st->x509.data = st->certificate->pbCertEncoded;
	st->x509.len = st->certificate->cbCertEncoded;

	/* Handle initial certificate validation */

	if (st->cert_chain->TrustStatus.dwErrorStatus != CERT_TRUST_NO_ERROR)
		return set_certificate_lookup_error(st->cert_chain->TrustStatus.dwErrorStatus);

	ssl_policy_parameters.cbSize = sizeof(SSL_EXTRA_CERT_CHAIN_POLICY_PARA);
	ssl_policy_parameters.dwAuthType = AUTHTYPE_SERVER;
	ssl_policy_parameters.pwszServerName = st->host_w;

	if (!CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL,
			st->cert_chain, &cert_policy_parameters,
			&cert_policy_status)) {
		git_error_set(GIT_ERROR_OS, "could not verify certificate chain policy");
		return -1;
	}

	if (cert_policy_status.dwError != SEC_E_OK)
		return set_certificate_validation_error(cert_policy_status.dwError);

	return 0;
}

static int schannel_connect(git_stream *stream)
{
	schannel_stream *st = (schannel_stream *)stream;
	int error;

	GIT_ASSERT(st->state == STATE_NONE);

	if ((error = connect_context(st)) < 0 ||
	    (error = check_certificate(st)) < 0)
		return error;

	st->connected = 1;
	return 0;
}

static int schannel_certificate(git_cert **out, git_stream *stream)
{
	schannel_stream *st = (schannel_stream *)stream;

	*out = &st->x509.parent;
	return 0;
}

static int schannel_set_proxy(
	git_stream *stream,
	const git_proxy_options *proxy_options)
{
	schannel_stream *st = (schannel_stream *)stream;
	return git_stream_set_proxy(st->io, proxy_options);
}

static ssize_t schannel_write(
	git_stream *stream,
	const char *data,
	size_t data_len,
	int flags)
{
	schannel_stream *st = (schannel_stream *)stream;
	SecBuffer encrypt_buf[3];
	SecBufferDesc encrypt_buf_desc = { SECBUFFER_VERSION, 3, encrypt_buf };
	git_str ciphertext_out = GIT_STR_INIT;
	ssize_t total_len = 0;

	GIT_UNUSED(flags);

	if (data_len > SSIZE_MAX)
		data_len = SSIZE_MAX;

	git_str_init(&ciphertext_out,
		st->stream_sizes.cbHeader +
		st->stream_sizes.cbMaximumMessage +
		st->stream_sizes.cbTrailer);

	while (data_len > 0) {
		size_t message_len = min(data_len, st->stream_sizes.cbMaximumMessage);
		size_t ciphertext_len, ciphertext_written = 0;

		encrypt_buf[0].BufferType = SECBUFFER_STREAM_HEADER;
		encrypt_buf[0].cbBuffer = st->stream_sizes.cbHeader;
		encrypt_buf[0].pvBuffer = ciphertext_out.ptr;

		encrypt_buf[1].BufferType = SECBUFFER_DATA;
		encrypt_buf[1].cbBuffer = (unsigned long)message_len;
		encrypt_buf[1].pvBuffer =
			ciphertext_out.ptr + st->stream_sizes.cbHeader;

		encrypt_buf[2].BufferType = SECBUFFER_STREAM_TRAILER;
		encrypt_buf[2].cbBuffer = st->stream_sizes.cbTrailer;
		encrypt_buf[2].pvBuffer =
			ciphertext_out.ptr + st->stream_sizes.cbHeader +
			message_len;

		memcpy(ciphertext_out.ptr + st->stream_sizes.cbHeader, data, message_len);

		if (EncryptMessage(&st->context, 0, &encrypt_buf_desc, 0) != SEC_E_OK) {
			git_error_set(GIT_ERROR_OS, "could not encrypt tls message");
			total_len = -1;
			goto done;
		}

		ciphertext_len = encrypt_buf[0].cbBuffer +
		                 encrypt_buf[1].cbBuffer +
		                 encrypt_buf[2].cbBuffer;

		while (ciphertext_written < ciphertext_len) {
			ssize_t chunk_len = git_stream_write(st->io,
				ciphertext_out.ptr + ciphertext_written,
				ciphertext_len - ciphertext_written, 0);

			if (chunk_len < 0) {
				total_len = -1;
				goto done;
			}

			ciphertext_len -= chunk_len;
			ciphertext_written += chunk_len;
		}

		total_len += message_len;

		data += message_len;
		data_len -= message_len;
	}

done:
	git_str_dispose(&ciphertext_out);
	return total_len;
}

static ssize_t schannel_read(git_stream *stream, void *_data, size_t data_len)
{
	schannel_stream *st = (schannel_stream *)stream;
	char *data = (char *)_data;
	SecBuffer decrypt_buf[4];
	SecBufferDesc decrypt_buf_desc = { SECBUFFER_VERSION, 4, decrypt_buf };
	SECURITY_STATUS status;
	ssize_t chunk_len, total_len = 0;

	if (data_len > SSIZE_MAX)
		data_len = SSIZE_MAX;

	/*
	 * Loop until we have some bytes to return - we may have decrypted
	 * bytes queued or ciphertext from the wire that we can decrypt and
	 * return. Return any queued bytes if they're available to avoid a
	 * network read, which may block. We may return less than the
	 * caller requested, and they can retry for an actual network
	 */
	while ((size_t)total_len < data_len) {
		if (st->plaintext_in.size > 0) {
			size_t copy_len = min(st->plaintext_in.size, data_len);

			memcpy(data, st->plaintext_in.ptr, copy_len);
			git_str_consume_bytes(&st->plaintext_in, copy_len);

			data += copy_len;
			data_len -= copy_len;

			total_len += copy_len;

			continue;
		}

		if (st->ciphertext_in.size > 0) {
			decrypt_buf[0].BufferType = SECBUFFER_DATA;
			decrypt_buf[0].cbBuffer = (unsigned long)min(st->ciphertext_in.size, ULONG_MAX);
			decrypt_buf[0].pvBuffer = st->ciphertext_in.ptr;

			decrypt_buf[1].BufferType = SECBUFFER_EMPTY;
			decrypt_buf[1].cbBuffer = 0;
			decrypt_buf[1].pvBuffer = NULL;

			decrypt_buf[2].BufferType = SECBUFFER_EMPTY;
			decrypt_buf[2].cbBuffer = 0;
			decrypt_buf[2].pvBuffer = NULL;

			decrypt_buf[3].BufferType = SECBUFFER_EMPTY;
			decrypt_buf[3].cbBuffer = 0;
			decrypt_buf[3].pvBuffer = NULL;

			status = DecryptMessage(&st->context, &decrypt_buf_desc, 0, NULL);

			if (status == SEC_E_OK) {
				GIT_ASSERT(decrypt_buf[0].BufferType == SECBUFFER_STREAM_HEADER);
				GIT_ASSERT(decrypt_buf[1].BufferType == SECBUFFER_DATA);
				GIT_ASSERT(decrypt_buf[2].BufferType == SECBUFFER_STREAM_TRAILER);

				if (git_str_put(&st->plaintext_in, decrypt_buf[1].pvBuffer, decrypt_buf[1].cbBuffer) < 0) {
					total_len = -1;
					goto done;
				}

				if (decrypt_buf[3].BufferType == SECBUFFER_EXTRA) {
					git_str_consume_bytes(&st->ciphertext_in, (st->ciphertext_in.size - decrypt_buf[3].cbBuffer));
				} else {
					git_str_clear(&st->ciphertext_in);
				}

				continue;
			} else if (status == SEC_E_CONTEXT_EXPIRED) {
				break;
			} else if (status != SEC_E_INCOMPLETE_MESSAGE) {
				git_error_set(GIT_ERROR_SSL, "could not decrypt tls message");
				total_len = -1;
				goto done;
			}
		}

		if (total_len != 0)
			break;

		if (git_str_grow_by(&st->ciphertext_in, READ_BLOCKSIZE) < 0) {
			total_len = -1;
			goto done;
		}

		if ((chunk_len = git_stream_read(st->io, st->ciphertext_in.ptr + st->ciphertext_in.size, st->ciphertext_in.asize - st->ciphertext_in.size)) < 0) {
			total_len = -1;
			goto done;
		}

		st->ciphertext_in.size += chunk_len;
	}

done:
	return total_len;
}

static int schannel_close(git_stream *stream)
{
	schannel_stream *st = (schannel_stream *)stream;
	int error = 0;

	if (st->connected) {
		SecBuffer shutdown_buf;
		SecBufferDesc shutdown_buf_desc =
			{ SECBUFFER_VERSION, 1, &shutdown_buf };
		DWORD shutdown_message = SCHANNEL_SHUTDOWN, shutdown_flags;

		shutdown_buf.BufferType = SECBUFFER_TOKEN;
		shutdown_buf.cbBuffer = sizeof(DWORD);
		shutdown_buf.pvBuffer = &shutdown_message;

		if (ApplyControlToken(&st->context, &shutdown_buf_desc) != SEC_E_OK) {
			git_error_set(GIT_ERROR_SSL, "could not shutdown stream");
			error = -1;
		}

		shutdown_buf.BufferType = SECBUFFER_TOKEN;
		shutdown_buf.cbBuffer = 0;
		shutdown_buf.pvBuffer = NULL;

		shutdown_flags = ISC_REQ_ALLOCATE_MEMORY |
		                 ISC_REQ_CONFIDENTIALITY |
		                 ISC_REQ_REPLAY_DETECT |
		                 ISC_REQ_SEQUENCE_DETECT |
		                 ISC_REQ_STREAM;

		if (InitializeSecurityContext(&st->cred, &st->context,
				NULL, shutdown_flags, 0, 0,
				&shutdown_buf_desc, 0, NULL,
				&shutdown_buf_desc, &shutdown_flags,
				NULL) == SEC_E_OK) {
			if (shutdown_buf.cbBuffer > 0) {
				if (git_stream__write_full(st->io,
						shutdown_buf.pvBuffer,
						shutdown_buf.cbBuffer, 0) < 0)
					error = -1;

				FreeContextBuffer(shutdown_buf.pvBuffer);
			}
		}
	}

	st->connected = false;

	if (st->owned && git_stream_close(st->io) < 0)
		error = -1;

	return error;
}

static void schannel_free(git_stream *stream)
{
	schannel_stream *st = (schannel_stream *)stream;

	if (st->state >= STATE_CERTIFICATE) {
		CertFreeCertificateContext(st->certificate);
		CertFreeCertificateChain(st->cert_chain);
	}

	if (st->state >= STATE_CONTEXT)
		DeleteSecurityContext(&st->context);

	if (st->state >= STATE_CRED)
		FreeCredentialsHandle(&st->cred);

	st->state = STATE_NONE;

	git_str_dispose(&st->ciphertext_in);
	git_str_dispose(&st->plaintext_in);

	git__free(st->host_w);

	if (st->owned)
		git_stream_free(st->io);

	git__free(st);
}

static int schannel_stream_wrap(
	git_stream **out,
	git_stream *in,
	const char *host,
	int owned)
{
	schannel_stream *st;

	st = git__calloc(1, sizeof(schannel_stream));
	GIT_ERROR_CHECK_ALLOC(st);

	st->io = in;
	st->owned = owned;

	if (git_utf8_to_16_alloc(&st->host_w, host) < 0) {
		git__free(st);
		return -1;
	}

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.encrypted = 1;
	st->parent.proxy_support = git_stream_supports_proxy(st->io);
	st->parent.connect = schannel_connect;
	st->parent.certificate = schannel_certificate;
	st->parent.set_proxy = schannel_set_proxy;
	st->parent.read = schannel_read;
	st->parent.write = schannel_write;
	st->parent.close = schannel_close;
	st->parent.free = schannel_free;

	*out = (git_stream *)st;
	return 0;
}

extern int git_schannel_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	git_stream *stream;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	if ((error = git_socket_stream_new(&stream, host, port)) < 0)
		return error;

	if ((error = schannel_stream_wrap(out, stream, host, 1)) < 0) {
		git_stream_close(stream);
		git_stream_free(stream);
	}

	return error;
}

extern int git_schannel_stream_wrap(
	git_stream **out,
	git_stream *in,
	const char *host)
{
	return schannel_stream_wrap(out, in, host, 0);
}

#endif
