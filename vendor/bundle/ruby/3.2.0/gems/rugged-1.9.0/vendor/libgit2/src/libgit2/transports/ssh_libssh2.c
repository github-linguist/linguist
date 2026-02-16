/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "ssh_libssh2.h"

#ifdef GIT_SSH_LIBSSH2

#include <libssh2.h>

#include "runtime.h"
#include "net.h"
#include "smart.h"
#include "process.h"
#include "streams/socket.h"
#include "sysdir.h"

#include "git2/credential.h"
#include "git2/sys/credential.h"

#define OWNING_SUBTRANSPORT(s) ((ssh_subtransport *)(s)->parent.subtransport)

extern int git_socket_stream__timeout;

static const char cmd_uploadpack[] = "git-upload-pack";
static const char cmd_receivepack[] = "git-receive-pack";

typedef struct {
	git_smart_subtransport_stream parent;
	git_stream *io;
	LIBSSH2_SESSION *session;
	LIBSSH2_CHANNEL *channel;
	const char *cmd;
	git_net_url url;
	unsigned sent_command : 1;
} ssh_stream;

typedef struct {
	git_smart_subtransport parent;
	transport_smart *owner;
	ssh_stream *current_stream;
	git_credential *cred;
	char *cmd_uploadpack;
	char *cmd_receivepack;
} ssh_subtransport;

static int list_auth_methods(int *out, LIBSSH2_SESSION *session, const char *username);

static void ssh_error(LIBSSH2_SESSION *session, const char *errmsg)
{
	char *ssherr;
	libssh2_session_last_error(session, &ssherr, NULL, 0);

	git_error_set(GIT_ERROR_SSH, "%s: %s", errmsg, ssherr);
}

/*
 * Create a git protocol request.
 *
 * For example: git-upload-pack '/libgit2/libgit2'
 */
static int gen_proto(git_str *request, const char *cmd, git_net_url *url)
{
	const char *repo;

	repo = url->path;

	if (repo && repo[0] == '/' && repo[1] == '~')
		repo++;

	if (!repo || !repo[0]) {
		git_error_set(GIT_ERROR_NET, "malformed git protocol URL");
		return -1;
	}

	git_str_puts(request, cmd);
	git_str_puts(request, " '");
	git_str_puts(request, repo);
	git_str_puts(request, "'");

	if (git_str_oom(request))
		return -1;

	return 0;
}

static int send_command(ssh_stream *s)
{
	int error;
	git_str request = GIT_STR_INIT;

	error = gen_proto(&request, s->cmd, &s->url);
	if (error < 0)
		goto cleanup;

	error = libssh2_channel_exec(s->channel, request.ptr);
	if (error < LIBSSH2_ERROR_NONE) {
		ssh_error(s->session, "SSH could not execute request");
		goto cleanup;
	}

	s->sent_command = 1;

cleanup:
	git_str_dispose(&request);
	return error;
}

static int ssh_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	ssh_stream *s = GIT_CONTAINER_OF(stream, ssh_stream, parent);
	ssize_t rc;

	*bytes_read = 0;

	if (!s->sent_command && send_command(s) < 0)
		return -1;

	if ((rc = libssh2_channel_read(s->channel, buffer, buf_size)) < LIBSSH2_ERROR_NONE) {
		ssh_error(s->session, "SSH could not read data");
		return -1;
	}

	/*
	 * If we can't get anything out of stdout, it's typically a
	 * not-found error, so read from stderr and signal EOF on
	 * stderr.
	 */
	if (rc == 0) {
		if ((rc = libssh2_channel_read_stderr(s->channel, buffer, buf_size)) > 0) {
			git_error_set(GIT_ERROR_SSH, "%*s", (int)rc, buffer);
			return GIT_EEOF;
		} else if (rc < LIBSSH2_ERROR_NONE) {
			ssh_error(s->session, "SSH could not read stderr");
			return -1;
		}
	}

	*bytes_read = rc;

	return 0;
}

static int ssh_stream_write(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	ssh_stream *s = GIT_CONTAINER_OF(stream, ssh_stream, parent);
	size_t off = 0;
	ssize_t ret = 0;

	if (!s->sent_command && send_command(s) < 0)
		return -1;

	do {
		ret = libssh2_channel_write(s->channel, buffer + off, len - off);
		if (ret < 0)
			break;

		off += ret;

	} while (off < len);

	if (ret < 0) {
		ssh_error(s->session, "SSH could not write data");
		return -1;
	}

	return 0;
}

static void ssh_stream_free(git_smart_subtransport_stream *stream)
{
	ssh_stream *s = GIT_CONTAINER_OF(stream, ssh_stream, parent);
	ssh_subtransport *t;

	if (!stream)
		return;

	t = OWNING_SUBTRANSPORT(s);
	t->current_stream = NULL;

	if (s->channel) {
		libssh2_channel_close(s->channel);
		libssh2_channel_free(s->channel);
		s->channel = NULL;
	}

	if (s->session) {
		libssh2_session_disconnect(s->session, "closing transport");
		libssh2_session_free(s->session);
		s->session = NULL;
	}

	if (s->io) {
		git_stream_close(s->io);
		git_stream_free(s->io);
		s->io = NULL;
	}

	git_net_url_dispose(&s->url);
	git__free(s);
}

static int ssh_stream_alloc(
	ssh_subtransport *t,
	const char *cmd,
	git_smart_subtransport_stream **stream)
{
	ssh_stream *s;

	GIT_ASSERT_ARG(stream);

	s = git__calloc(sizeof(ssh_stream), 1);
	GIT_ERROR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = ssh_stream_read;
	s->parent.write = ssh_stream_write;
	s->parent.free = ssh_stream_free;

	s->cmd = cmd;

	*stream = &s->parent;
	return 0;
}

static int ssh_agent_auth(LIBSSH2_SESSION *session, git_credential_ssh_key *c) {
	int rc = LIBSSH2_ERROR_NONE;

	struct libssh2_agent_publickey *curr, *prev = NULL;

	LIBSSH2_AGENT *agent = libssh2_agent_init(session);

	if (agent == NULL)
		return -1;

	rc = libssh2_agent_connect(agent);

	if (rc != LIBSSH2_ERROR_NONE) {
		rc = LIBSSH2_ERROR_AUTHENTICATION_FAILED;
		goto shutdown;
	}

	rc = libssh2_agent_list_identities(agent);

	if (rc != LIBSSH2_ERROR_NONE)
		goto shutdown;

	while (1) {
		rc = libssh2_agent_get_identity(agent, &curr, prev);

		if (rc < 0)
			goto shutdown;

		/* rc is set to 1 whenever the ssh agent ran out of keys to check.
		 * Set the error code to authentication failure rather than erroring
		 * out with an untranslatable error code.
		 */
		if (rc == 1) {
			rc = LIBSSH2_ERROR_AUTHENTICATION_FAILED;
			goto shutdown;
		}

		rc = libssh2_agent_userauth(agent, c->username, curr);

		if (rc == 0)
			break;

		prev = curr;
	}

shutdown:

	if (rc != LIBSSH2_ERROR_NONE)
		ssh_error(session, "error authenticating");

	libssh2_agent_disconnect(agent);
	libssh2_agent_free(agent);

	return rc;
}

static int _git_ssh_authenticate_session(
	LIBSSH2_SESSION *session,
	git_credential *cred)
{
	int rc;

	do {
		git_error_clear();
		switch (cred->credtype) {
		case GIT_CREDENTIAL_USERPASS_PLAINTEXT: {
			git_credential_userpass_plaintext *c = (git_credential_userpass_plaintext *)cred;
			rc = libssh2_userauth_password(session, c->username, c->password);
			break;
		}
		case GIT_CREDENTIAL_SSH_KEY: {
			git_credential_ssh_key *c = (git_credential_ssh_key *)cred;

			if (c->privatekey)
				rc = libssh2_userauth_publickey_fromfile(
					session, c->username, c->publickey,
					c->privatekey, c->passphrase);
			else
				rc = ssh_agent_auth(session, c);

			break;
		}
		case GIT_CREDENTIAL_SSH_CUSTOM: {
			git_credential_ssh_custom *c = (git_credential_ssh_custom *)cred;

			rc = libssh2_userauth_publickey(
				session, c->username, (const unsigned char *)c->publickey,
				c->publickey_len, c->sign_callback, &c->payload);
			break;
		}
		case GIT_CREDENTIAL_SSH_INTERACTIVE: {
			void **abstract = libssh2_session_abstract(session);
			git_credential_ssh_interactive *c = (git_credential_ssh_interactive *)cred;

			/* ideally, we should be able to set this by calling
			 * libssh2_session_init_ex() instead of libssh2_session_init().
			 * libssh2's API is inconsistent here i.e. libssh2_userauth_publickey()
			 * allows you to pass the `abstract` as part of the call, whereas
			 * libssh2_userauth_keyboard_interactive() does not!
			 *
			 * The only way to set the `abstract` pointer is by calling
			 * libssh2_session_abstract(), which will replace the existing
			 * pointer as is done below. This is safe for now (at time of writing),
			 * but may not be valid in future.
			 */
			*abstract = c->payload;

			rc = libssh2_userauth_keyboard_interactive(
				session, c->username, c->prompt_callback);
			break;
		}
#ifdef GIT_SSH_LIBSSH2_MEMORY_CREDENTIALS
		case GIT_CREDENTIAL_SSH_MEMORY: {
			git_credential_ssh_key *c = (git_credential_ssh_key *)cred;

			GIT_ASSERT(c->username);
			GIT_ASSERT(c->privatekey);

			rc = libssh2_userauth_publickey_frommemory(
				session,
				c->username,
				strlen(c->username),
				c->publickey,
				c->publickey ? strlen(c->publickey) : 0,
				c->privatekey,
				strlen(c->privatekey),
				c->passphrase);
			break;
		}
#endif
		default:
			rc = LIBSSH2_ERROR_AUTHENTICATION_FAILED;
		}
	} while (LIBSSH2_ERROR_EAGAIN == rc || LIBSSH2_ERROR_TIMEOUT == rc);

	if (rc == LIBSSH2_ERROR_PASSWORD_EXPIRED ||
		rc == LIBSSH2_ERROR_AUTHENTICATION_FAILED ||
		rc == LIBSSH2_ERROR_PUBLICKEY_UNVERIFIED)
			return GIT_EAUTH;

	if (rc != LIBSSH2_ERROR_NONE) {
		if (git_error_last()->klass == GIT_ERROR_NONE)
			ssh_error(session, "failed to authenticate SSH session");
		return -1;
	}

	return 0;
}

static int request_creds(git_credential **out, ssh_subtransport *t, const char *user, int auth_methods)
{
	int error, no_callback = 0;
	git_credential *cred = NULL;

	if (!t->owner->connect_opts.callbacks.credentials) {
		no_callback = 1;
	} else {
		error = t->owner->connect_opts.callbacks.credentials(
			&cred,
			t->owner->url,
			user,
			auth_methods,
			t->owner->connect_opts.callbacks.payload);

		if (error == GIT_PASSTHROUGH) {
			no_callback = 1;
		} else if (error < 0) {
			return error;
		} else if (!cred) {
			git_error_set(GIT_ERROR_SSH, "callback failed to initialize SSH credentials");
			return -1;
		}
	}

	if (no_callback) {
		git_error_set(GIT_ERROR_SSH, "authentication required but no callback set");
		return GIT_EAUTH;
	}

	if (!(cred->credtype & auth_methods)) {
		cred->free(cred);
		git_error_set(GIT_ERROR_SSH, "authentication callback returned unsupported credentials type");
		return GIT_EAUTH;
	}

	*out = cred;

	return 0;
}

#define SSH_DIR          ".ssh"
#define KNOWN_HOSTS_FILE "known_hosts"

/*
 * Load the known_hosts file.
 *
 * Returns success but leaves the output NULL if we couldn't find the file.
 */
static int load_known_hosts(LIBSSH2_KNOWNHOSTS **hosts, LIBSSH2_SESSION *session)
{
	git_str path = GIT_STR_INIT, sshdir = GIT_STR_INIT;
	LIBSSH2_KNOWNHOSTS *known_hosts = NULL;
	int error;

	GIT_ASSERT_ARG(hosts);

	if ((error = git_sysdir_expand_homedir_file(&sshdir, SSH_DIR)) < 0 ||
	    (error = git_str_joinpath(&path, git_str_cstr(&sshdir), KNOWN_HOSTS_FILE)) < 0)
		goto out;

	if ((known_hosts = libssh2_knownhost_init(session)) == NULL) {
		ssh_error(session, "error initializing known hosts");
		error = -1;
		goto out;
	}

	/*
	 * Try to read the file and consider not finding it as not trusting the
	 * host rather than an error.
	 */
	error = libssh2_knownhost_readfile(known_hosts, git_str_cstr(&path), LIBSSH2_KNOWNHOST_FILE_OPENSSH);
	if (error == LIBSSH2_ERROR_FILE)
		error = 0;
	if (error < 0)
		ssh_error(session, "error reading known_hosts");

out:
	*hosts = known_hosts;

	git_str_dispose(&sshdir);
	git_str_dispose(&path);

	return error;
}

static void add_hostkey_pref_if_avail(
	LIBSSH2_KNOWNHOSTS *known_hosts,
	const char *hostname,
	int port,
	git_str *prefs,
	int type,
	const char *type_name)
{
	struct libssh2_knownhost *host = NULL;
	const char key = '\0';
	int mask = LIBSSH2_KNOWNHOST_TYPE_PLAIN | LIBSSH2_KNOWNHOST_KEYENC_RAW | type;
	int error;

	error = libssh2_knownhost_checkp(known_hosts, hostname, port, &key, 1, mask, &host);
	if (error == LIBSSH2_KNOWNHOST_CHECK_MISMATCH) {
		if (git_str_len(prefs) > 0) {
			git_str_putc(prefs, ',');
		}
		git_str_puts(prefs, type_name);
	}
}

/*
 * We figure out what kind of key we want to ask the remote for by trying to
 * look it up with a nonsense key and using that mismatch to figure out what key
 * we do have stored for the host.
 *
 * Populates prefs with the string to pass to libssh2_session_method_pref.
 */
static void find_hostkey_preference(
	LIBSSH2_KNOWNHOSTS *known_hosts,
	const char *hostname,
	int port,
	git_str *prefs)
{
	/*
	 * The order here is important as it indicates the priority of what will
	 * be preferred.
	 */
#ifdef LIBSSH2_KNOWNHOST_KEY_ED25519
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_ED25519, "ssh-ed25519");
#endif
#ifdef LIBSSH2_KNOWNHOST_KEY_ECDSA_256
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_ECDSA_256, "ecdsa-sha2-nistp256");
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_ECDSA_384, "ecdsa-sha2-nistp384");
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_ECDSA_521, "ecdsa-sha2-nistp521");
#endif
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_SSHRSA, "rsa-sha2-512");
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_SSHRSA, "rsa-sha2-256");
	add_hostkey_pref_if_avail(known_hosts, hostname, port, prefs, LIBSSH2_KNOWNHOST_KEY_SSHRSA, "ssh-rsa");
}

static int _git_ssh_session_create(
	LIBSSH2_SESSION **session,
	LIBSSH2_KNOWNHOSTS **hosts,
	const char *hostname,
	int port,
	git_stream *io)
{
	git_socket_stream *socket = GIT_CONTAINER_OF(io, git_socket_stream, parent);
	LIBSSH2_SESSION *s;
	LIBSSH2_KNOWNHOSTS *known_hosts;
	git_str prefs = GIT_STR_INIT;
	int rc = 0;

	GIT_ASSERT_ARG(session);
	GIT_ASSERT_ARG(hosts);

	s = libssh2_session_init();
	if (!s) {
		git_error_set(GIT_ERROR_NET, "failed to initialize SSH session");
		return -1;
	}

	if (git_socket_stream__timeout > 0) {
		libssh2_session_set_timeout(s, git_socket_stream__timeout);
	}

	if ((rc = load_known_hosts(&known_hosts, s)) < 0) {
		ssh_error(s, "error loading known_hosts");
		libssh2_session_free(s);
		return -1;
	}

	find_hostkey_preference(known_hosts, hostname, port, &prefs);
	if (git_str_len(&prefs) > 0) {
		do {
			rc = libssh2_session_method_pref(s, LIBSSH2_METHOD_HOSTKEY, git_str_cstr(&prefs));
		} while (LIBSSH2_ERROR_EAGAIN == rc || LIBSSH2_ERROR_TIMEOUT == rc);
		if (rc != LIBSSH2_ERROR_NONE) {
			ssh_error(s, "failed to set hostkey preference");
			goto on_error;
		}
	}
	git_str_dispose(&prefs);

	do {
		rc = libssh2_session_handshake(s, socket->s);
	} while (LIBSSH2_ERROR_EAGAIN == rc || LIBSSH2_ERROR_TIMEOUT == rc);

	if (rc != LIBSSH2_ERROR_NONE) {
		ssh_error(s, "failed to start SSH session");
		goto on_error;
	}

	libssh2_session_set_blocking(s, 1);

	*session = s;
	*hosts = known_hosts;

	return 0;

on_error:
	libssh2_knownhost_free(known_hosts);
	libssh2_session_free(s);
	return -1;
}


/*
 * Returns the typemask argument to pass to libssh2_knownhost_check{,p} based on
 * the type of key that libssh2_session_hostkey returns.
 */
static int fingerprint_type_mask(int keytype)
{
	int mask = LIBSSH2_KNOWNHOST_TYPE_PLAIN | LIBSSH2_KNOWNHOST_KEYENC_RAW;
	return mask;

	switch (keytype) {
	case LIBSSH2_HOSTKEY_TYPE_RSA:
		mask |= LIBSSH2_KNOWNHOST_KEY_SSHRSA;
		break;
	case LIBSSH2_HOSTKEY_TYPE_DSS:
		mask |= LIBSSH2_KNOWNHOST_KEY_SSHDSS;
		break;
#ifdef LIBSSH2_HOSTKEY_TYPE_ECDSA_256
	case LIBSSH2_HOSTKEY_TYPE_ECDSA_256:
		mask |= LIBSSH2_KNOWNHOST_KEY_ECDSA_256;
		break;
	case LIBSSH2_HOSTKEY_TYPE_ECDSA_384:
		mask |= LIBSSH2_KNOWNHOST_KEY_ECDSA_384;
		break;
	case LIBSSH2_HOSTKEY_TYPE_ECDSA_521:
		mask |= LIBSSH2_KNOWNHOST_KEY_ECDSA_521;
		break;
#endif
#ifdef LIBSSH2_HOSTKEY_TYPE_ED25519
	case LIBSSH2_HOSTKEY_TYPE_ED25519:
		mask |= LIBSSH2_KNOWNHOST_KEY_ED25519;
		break;
#endif
	}

	return mask;
}

/*
 * Check the host against the user's known_hosts file.
 *
 * Returns 1/0 for valid/''not-valid or <0 for an error
 */
static int check_against_known_hosts(
	LIBSSH2_SESSION *session,
	LIBSSH2_KNOWNHOSTS *known_hosts,
	const char *hostname,
	int port,
	const char *key,
	size_t key_len,
	int key_type)
{
	int check, typemask, ret = 0;
	struct libssh2_knownhost *host = NULL;

	if (known_hosts == NULL)
		return 0;

	typemask = fingerprint_type_mask(key_type);
	check = libssh2_knownhost_checkp(known_hosts, hostname, port, key, key_len, typemask, &host);
	if (check == LIBSSH2_KNOWNHOST_CHECK_FAILURE) {
		ssh_error(session, "error checking for known host");
		return -1;
	}

	ret = check == LIBSSH2_KNOWNHOST_CHECK_MATCH ? 1 : 0;

	return ret;
}

/*
 * Perform the check for the session's certificate against known hosts if
 * possible and then ask the user if they have a callback.
 *
 * Returns 1/0 for valid/not-valid or <0 for an error
 */
static int check_certificate(
	LIBSSH2_SESSION *session,
	LIBSSH2_KNOWNHOSTS *known_hosts,
	git_transport_certificate_check_cb check_cb,
	void *check_cb_payload,
	const char *host,
	int port)
{
	git_cert_hostkey cert = {{ 0 }};
	const char *key;
	size_t cert_len;
	int cert_type, cert_valid = 0, error = GIT_ECERTIFICATE;

	if ((key = libssh2_session_hostkey(session, &cert_len, &cert_type)) == NULL) {
		ssh_error(session, "failed to retrieve hostkey");
		return -1;
	}

	if ((cert_valid = check_against_known_hosts(session, known_hosts, host, port, key, cert_len, cert_type)) < 0)
		return -1;

	cert.parent.cert_type = GIT_CERT_HOSTKEY_LIBSSH2;
	if (key != NULL) {
		cert.type |= GIT_CERT_SSH_RAW;
		cert.hostkey = key;
		cert.hostkey_len = cert_len;
		switch (cert_type) {
		case LIBSSH2_HOSTKEY_TYPE_RSA:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_RSA;
			break;
		case LIBSSH2_HOSTKEY_TYPE_DSS:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_DSS;
			break;

#ifdef LIBSSH2_HOSTKEY_TYPE_ECDSA_256
		case LIBSSH2_HOSTKEY_TYPE_ECDSA_256:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_256;
			break;
		case LIBSSH2_HOSTKEY_TYPE_ECDSA_384:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_384;
			break;
		case LIBSSH2_KNOWNHOST_KEY_ECDSA_521:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_521;
			break;
#endif

#ifdef LIBSSH2_HOSTKEY_TYPE_ED25519
		case LIBSSH2_HOSTKEY_TYPE_ED25519:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_KEY_ED25519;
			break;
#endif
		default:
			cert.raw_type = GIT_CERT_SSH_RAW_TYPE_UNKNOWN;
		}
	}

#ifdef LIBSSH2_HOSTKEY_HASH_SHA256
	key = libssh2_hostkey_hash(session, LIBSSH2_HOSTKEY_HASH_SHA256);
	if (key != NULL) {
		cert.type |= GIT_CERT_SSH_SHA256;
		memcpy(&cert.hash_sha256, key, 32);
	}
#endif

	key = libssh2_hostkey_hash(session, LIBSSH2_HOSTKEY_HASH_SHA1);
	if (key != NULL) {
		cert.type |= GIT_CERT_SSH_SHA1;
		memcpy(&cert.hash_sha1, key, 20);
	}

	key = libssh2_hostkey_hash(session, LIBSSH2_HOSTKEY_HASH_MD5);
	if (key != NULL) {
		cert.type |= GIT_CERT_SSH_MD5;
		memcpy(&cert.hash_md5, key, 16);
	}

	if (cert.type == 0) {
		git_error_set(GIT_ERROR_SSH, "unable to get the host key");
		return -1;
	}

	if (check_cb != NULL) {
		git_cert_hostkey *cert_ptr = &cert;

		error = check_cb((git_cert *)cert_ptr, cert_valid, host,
			         check_cb_payload);

		if (error == 0)
			cert_valid = 1;
		else if (error != GIT_PASSTHROUGH)
			cert_valid = 0;
	}

	if (!cert_valid) {
		git_error_set(GIT_ERROR_SSH, "invalid or unknown remote ssh hostkey");
		return (error == GIT_PASSTHROUGH) ? GIT_ECERTIFICATE : error;
	}

	return 0;
}

#define SSH_DEFAULT_PORT "22"

static int _git_ssh_setup_conn(
	ssh_subtransport *t,
	const char *url,
	const char *cmd,
	git_smart_subtransport_stream **stream)
{
	int auth_methods, error = 0, port;
	ssh_stream *s;
	git_credential *cred = NULL;
	LIBSSH2_SESSION *session=NULL;
	LIBSSH2_CHANNEL *channel=NULL;
	LIBSSH2_KNOWNHOSTS *known_hosts = NULL;

	t->current_stream = NULL;

	*stream = NULL;
	if (ssh_stream_alloc(t, cmd, stream) < 0)
		return -1;

	s = (ssh_stream *)*stream;
	s->session = NULL;
	s->channel = NULL;

	if (git_net_str_is_url(url))
		error = git_net_url_parse(&s->url, url);
	else
		error = git_net_url_parse_scp(&s->url, url);

	if (error < 0)
		goto done;

	/* Safety check: like git, we forbid paths that look like an option as
	 * that could lead to injection on the remote side */
	if (git_process__is_cmdline_option(s->url.path)) {
		git_error_set(GIT_ERROR_NET, "cannot ssh: path '%s' is ambiguous with command-line option", s->url.path);
		error = -1;
		goto done;
	}


	if ((error = git_socket_stream_new(&s->io, s->url.host, s->url.port)) < 0 ||
	    (error = git_stream_connect(s->io)) < 0)
		goto done;

	/*
	 * Try to parse the port as a number, if we can't then fall back to
	 * default. It would be nice if we could get the port that was resolved
	 * as part of the stream connection, but that's not something that's
	 * exposed.
	 */
	if (git__strntol32(&port, s->url.port, strlen(s->url.port), NULL, 10) < 0)
		port = -1;

	if ((error = _git_ssh_session_create(&session, &known_hosts, s->url.host, port, s->io)) < 0)
		goto done;

	if ((error = check_certificate(session, known_hosts, t->owner->connect_opts.callbacks.certificate_check, t->owner->connect_opts.callbacks.payload, s->url.host, port)) < 0)
		goto done;

	/* we need the username to ask for auth methods */
	if (!s->url.username) {
		if ((error = request_creds(&cred, t, NULL, GIT_CREDENTIAL_USERNAME)) < 0)
			goto done;

		s->url.username = git__strdup(((git_credential_username *) cred)->username);
		cred->free(cred);
		cred = NULL;
		if (!s->url.username)
			goto done;
	} else if (s->url.username && s->url.password) {
		if ((error = git_credential_userpass_plaintext_new(&cred, s->url.username, s->url.password)) < 0)
			goto done;
	}

	if ((error = list_auth_methods(&auth_methods, session, s->url.username)) < 0)
		goto done;

	error = GIT_EAUTH;
	/* if we already have something to try */
	if (cred && auth_methods & cred->credtype)
		error = _git_ssh_authenticate_session(session, cred);

	while (error == GIT_EAUTH) {
		if (cred) {
			cred->free(cred);
			cred = NULL;
		}

		if ((error = request_creds(&cred, t, s->url.username, auth_methods)) < 0)
			goto done;

		if (strcmp(s->url.username, git_credential_get_username(cred))) {
			git_error_set(GIT_ERROR_SSH, "username does not match previous request");
			error = -1;
			goto done;
		}

		error = _git_ssh_authenticate_session(session, cred);

		if (error == GIT_EAUTH) {
			/* refresh auth methods */
			if ((error = list_auth_methods(&auth_methods, session, s->url.username)) < 0)
				goto done;
			else
				error = GIT_EAUTH;
		}
	}

	if (error < 0)
		goto done;

	channel = libssh2_channel_open_session(session);
	if (!channel) {
		error = -1;
		ssh_error(session, "Failed to open SSH channel");
		goto done;
	}

	libssh2_channel_set_blocking(channel, 1);

	s->session = session;
	s->channel = channel;

	t->current_stream = s;

done:
	if (known_hosts)
		libssh2_knownhost_free(known_hosts);

	if (error < 0) {
		ssh_stream_free(*stream);

		if (session)
			libssh2_session_free(session);
	}

	if (cred)
		cred->free(cred);

	return error;
}

static int ssh_uploadpack_ls(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	const char *cmd = t->cmd_uploadpack ? t->cmd_uploadpack : cmd_uploadpack;

	return _git_ssh_setup_conn(t, url, cmd, stream);
}

static int ssh_uploadpack(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	git_error_set(GIT_ERROR_NET, "must call UPLOADPACK_LS before UPLOADPACK");
	return -1;
}

static int ssh_receivepack_ls(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	const char *cmd = t->cmd_receivepack ? t->cmd_receivepack : cmd_receivepack;


	return _git_ssh_setup_conn(t, url, cmd, stream);
}

static int ssh_receivepack(
	ssh_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	git_error_set(GIT_ERROR_NET, "must call RECEIVEPACK_LS before RECEIVEPACK");
	return -1;
}

static int _ssh_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	ssh_subtransport *t = GIT_CONTAINER_OF(subtransport, ssh_subtransport, parent);

	switch (action) {
		case GIT_SERVICE_UPLOADPACK_LS:
			return ssh_uploadpack_ls(t, url, stream);

		case GIT_SERVICE_UPLOADPACK:
			return ssh_uploadpack(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK_LS:
			return ssh_receivepack_ls(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK:
			return ssh_receivepack(t, url, stream);
	}

	*stream = NULL;
	return -1;
}

static int _ssh_close(git_smart_subtransport *subtransport)
{
	ssh_subtransport *t = GIT_CONTAINER_OF(subtransport, ssh_subtransport, parent);

	GIT_ASSERT(!t->current_stream);

	GIT_UNUSED(t);

	return 0;
}

static void _ssh_free(git_smart_subtransport *subtransport)
{
	ssh_subtransport *t = GIT_CONTAINER_OF(subtransport, ssh_subtransport, parent);

	git__free(t->cmd_uploadpack);
	git__free(t->cmd_receivepack);
	git__free(t);
}

#define SSH_AUTH_PUBLICKEY "publickey"
#define SSH_AUTH_PASSWORD "password"
#define SSH_AUTH_KEYBOARD_INTERACTIVE "keyboard-interactive"

static int list_auth_methods(int *out, LIBSSH2_SESSION *session, const char *username)
{
	const char *list, *ptr;

	*out = 0;

	list = libssh2_userauth_list(session, username,
			(unsigned int)strlen(username));

	/* either error, or the remote accepts NONE auth, which is bizarre, let's punt */
	if (list == NULL && !libssh2_userauth_authenticated(session)) {
		ssh_error(session, "remote rejected authentication");
		return GIT_EAUTH;
	}

	ptr = list;
	while (ptr) {
		if (*ptr == ',')
			ptr++;

		if (!git__prefixcmp(ptr, SSH_AUTH_PUBLICKEY)) {
			*out |= GIT_CREDENTIAL_SSH_KEY;
			*out |= GIT_CREDENTIAL_SSH_CUSTOM;
#ifdef GIT_SSH_LIBSSH2_MEMORY_CREDENTIALS
			*out |= GIT_CREDENTIAL_SSH_MEMORY;
#endif
			ptr += strlen(SSH_AUTH_PUBLICKEY);
			continue;
		}

		if (!git__prefixcmp(ptr, SSH_AUTH_PASSWORD)) {
			*out |= GIT_CREDENTIAL_USERPASS_PLAINTEXT;
			ptr += strlen(SSH_AUTH_PASSWORD);
			continue;
		}

		if (!git__prefixcmp(ptr, SSH_AUTH_KEYBOARD_INTERACTIVE)) {
			*out |= GIT_CREDENTIAL_SSH_INTERACTIVE;
			ptr += strlen(SSH_AUTH_KEYBOARD_INTERACTIVE);
			continue;
		}

		/* Skip it if we don't know it */
		ptr = strchr(ptr, ',');
	}

	return 0;
}

int git_smart_subtransport_ssh_libssh2(
	git_smart_subtransport **out,
	git_transport *owner,
	void *param)
{
	ssh_subtransport *t;

	GIT_ASSERT_ARG(out);

	GIT_UNUSED(param);

	t = git__calloc(sizeof(ssh_subtransport), 1);
	GIT_ERROR_CHECK_ALLOC(t);

	t->owner = (transport_smart *)owner;
	t->parent.action = _ssh_action;
	t->parent.close = _ssh_close;
	t->parent.free = _ssh_free;

	*out = (git_smart_subtransport *) t;
	return 0;
}

int git_smart_subtransport_ssh_libssh2_set_paths(
	git_smart_subtransport *subtransport,
	const char *cmd_uploadpack,
	const char *cmd_receivepack)
{
	ssh_subtransport *t = (ssh_subtransport *)subtransport;

	git__free(t->cmd_uploadpack);
	git__free(t->cmd_receivepack);

	t->cmd_uploadpack = git__strdup(cmd_uploadpack);
	GIT_ERROR_CHECK_ALLOC(t->cmd_uploadpack);

	t->cmd_receivepack = git__strdup(cmd_receivepack);
	GIT_ERROR_CHECK_ALLOC(t->cmd_receivepack);

	return 0;
}

static void shutdown_libssh2(void)
{
    libssh2_exit();
}

int git_transport_ssh_libssh2_global_init(void)
{
	if (libssh2_init(0) < 0) {
		git_error_set(GIT_ERROR_SSH, "unable to initialize libssh2");
		return -1;
	}

	return git_runtime_shutdown_register(shutdown_libssh2);
}

#else /* GIT_SSH */

int git_transport_ssh_libssh2_global_init(void)
{
	return 0;
}

#endif
