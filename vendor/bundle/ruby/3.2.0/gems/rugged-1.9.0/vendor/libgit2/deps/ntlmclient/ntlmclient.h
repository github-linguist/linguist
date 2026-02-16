/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */
#ifndef INCLUDE_NTLMCLIENT_H__
#define INCLUDE_NTLMCLIENT_H__

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define NTLM_CLIENT_VERSION         "0.9.0"
#define NTLM_CLIENT_VERSION_MAJOR   0
#define NTLM_CLIENT_VERSION_MINOR   9
#define NTLM_CLIENT_VERSION_TEENY   0

typedef struct ntlm_client ntlm_client;

typedef enum {
	/**
	 * An error occurred; more details are available by querying
	 * `ntlm_client_errmsg`.
	 */
	NTLM_CLIENT_ERROR = -1,

	/**
	 * The input provided to the function is missing or invalid.
	 */
	NTLM_CLIENT_ERROR_INVALID_INPUT = -2
} ntlm_error_code;

/*
 * Flags for initializing the `ntlm_client` context.  A combination of
 * these flags can be provided to `ntlm_client_init`.
 */
typedef enum {
	/** Default settings for the `ntlm_client`. */
	NTLM_CLIENT_DEFAULTS               = 0,

	/**
	 * Disable Unicode negotiation.  By default, strings are converted
	 * into UTF-16 when supplied to the remote host, but if this flag
	 * is specified, localizable strings (like username and password)
	 * will only be sent to the server as they were provided to the
	 * library.  Since the NTLM protocol does not deliver the locale
	 * information, these will be interpreted by the remote host in
	 * whatever locale is configured, and likely be corrupted unless
	 * you limit yourself to ASCII.
	 *
	 * You are discouraged from setting this flag.
	 */
	NTLM_CLIENT_DISABLE_UNICODE        = (1 << 0),

	/*
	 * Enable LM ("Lan Manager") authentication support.  By default,
	 * LM authentication is disabled, since most remote servers have
	 * disabled support for it, and because it is both trivially
	 * brute-forced _and_ subject to rainbow table lookups.  If this
	 * flag is enabled, LM is still not used unless NTLM2 support is
	 * also disabled.
	 *
	 * You are discouraged from setting this flag.
	 */
	NTLM_CLIENT_ENABLE_LM              = (1 << 1),

	/*
	 * Enable NTLM ("Lan Manager") authentication support.  By default,
	 * NTLM authentication is disabled, since most remote servers have
	 * disabled support for it, due to its weakness.  If this flag is
	 * enabled, NTLM is still not used unless NTLM2 support is also
	 * disabled.
	 *
	 * You are discouraged from setting this flag.
	 */
	NTLM_CLIENT_ENABLE_NTLM            = (1 << 2),

	/*
	 * Disable NTLM2 authentication support.  By default, _only_ NTLM2
	 * support is enabled, since most remote servers will only support
	 * it due to its (relative) lack of weakness.  If this flag is
	 * set, either NTLM or LM (or both) must be explicitly enabled or
	 * there will be no mechanisms available to use.
	 *
	 * You are discouraged from setting this flag.
	 */
	NTLM_CLIENT_DISABLE_NTLM2          = (1 << 3),

	/*
	 * Request the target's name.  By default, you are expected to
	 * provide the name of the target you are authenticating to (eg,
	 * the remote hostname).  If set, the remote host will provide
	 * its idea of its hostname in the challenge message.  You may
	 * then set the authentication target based on it.
	 */
	NTLM_CLIENT_DISABLE_REQUEST_TARGET = (1 << 4)
} ntlm_client_flags;


/** Declare a public function exported for application use. */
#if __GNUC__ >= 4 && !defined(NTLM_STATIC)
# define NTLM_EXTERN(type) extern \
             __attribute__((visibility("default"))) \
             type
#elif defined(_MSC_VER) && !defined(NTLM_STATIC)
# define NTLM_EXTERN(type) __declspec(dllexport) type
#else
# define NTLM_EXTERN(type) extern type
#endif

/**
 * Initializes an `ntlm_client` context, which can begin sending
 * and receiving NTLM authentication messages.
 *
 * @param flags the `ntlm_client_flag_t`s to use for negotiation.
 * @return the `ntlm_client` context, or `NULL` if out-of-memory.
 */
NTLM_EXTERN(ntlm_client *) ntlm_client_init(ntlm_client_flags flags);

/**
 * Gets the error message for the most recent error that occurred.  If
 * a function returns an error, more details can be retrieved with this
 * function.  The string returned is a constant string; it should not
 * be freed.
 *
 * @return a constant string containing the error message.
 */
NTLM_EXTERN(const char *) ntlm_client_errmsg(ntlm_client *ntlm);

/**
 * Sets the local hostname and domain.  These strings should be in
 * ASCII.  They will be provided to the remote host during the
 * negotiation phase.
 *
 * @param ntlm the `ntlm_client` context to configure
 * @param hostname the hostname of the local machine
 * @param domain the domain of the local machine
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_set_hostname(
	ntlm_client *ntlm,
	const char *hostname,
	const char *domain);

/**
 * Sets the local operating system version.  These numbers are expected
 * to correspond to Windows operating system versions; for example
 * major version 6, minor version 2, build 9200 would correspond to
 * Windows 8 (aka "NT 6.2").
 *
 * It is not likely that you need to set the local version.
 *
 * @param ntlm the `ntlm_client` context to configure
 * @param major the major version number of the local operating system
 * @param minor the minor version number of the local operating system
 * @param build the build number of the local operating system
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_set_version(
	ntlm_client *ntlm,
	uint8_t major,
	uint8_t minor,
	uint16_t build);

/**
 * Sets the username and password to authenticate with to the remote
 * host.  Username and password may be specified in UTF-8 but the
 * domain should be in ASCII.  These will not be sent to the remote host
 * but will instead be used to compute the LM, NTLM or NTLM2 responses,
 * which will be provided to the remote host during the response phase.
 *
 * @param ntlm the `ntlm_client` context to configure
 * @param username the username to authenticate with
 * @param domain the domain of the user authenticating
 * @param password the password to authenticate with
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_set_credentials(
	ntlm_client *ntlm,
	const char *username,
	const char *domain,
	const char *password);

/**
 * Sets the authentication target, your idea of the remote host's
 * name.  The target should be provided as ASCII.  It will be
 * provided to the remote host during the response phase.
 *
 * @param ntlm the `ntlm_client` context to configure
 * @param target the name of the authentication target
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_set_target(
	ntlm_client *ntlm,
	const char *target);

/**
 * Gets the remote host's nonce, as it was provided in the challenge
 * message.  This is an opaque 8 byte value that is used to compute
 * the LM, NTLM and NTLM2 responses.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the challenge from the remote host
 */
NTLM_EXTERN(uint64_t) ntlm_client_challenge_nonce(
	ntlm_client *ntlm);

/**
 * Gets the remote hosts's target name, which can be used as the
 * authentication target.  This will be given as it was provided
 * in the challenge message.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the remote host's target name
 */
NTLM_EXTERN(const char *) ntlm_client_target(ntlm_client *ntlm);

/**
 * Gets the remote hosts's name, which is generally its short name.
 * This will be given as it was provided in the challenge message.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the remote host's server name
 */
NTLM_EXTERN(const char *) ntlm_client_target_server(ntlm_client *ntlm);

/**
 * Gets the remote hosts's domain, which is generally the short or
 * NT-style domain name.  This will be given as it was provided in
 * the challenge message.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the remote host's domain
 */
NTLM_EXTERN(const char *) ntlm_client_target_domain(ntlm_client *ntlm);

/**
 * Gets the remote hosts's DNS name, which is generally the long-style
 * Active Directory or fully-qualified hostname.  This will be given
 * as it was provided in the challenge message.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the remote host's DNS name
 */
NTLM_EXTERN(const char *) ntlm_client_target_server_dns(ntlm_client *ntlm);

/**
 * Gets the remote hosts's DNS domain, which is generally the long-style
 * Active Directory or fully-qualified domain name.  This will be given
 * as it was provided in the challenge message.
 *
 * @param ntlm the `ntlm_client` context to query
 * @return the remote host's DNS domain
 */
NTLM_EXTERN(const char *) ntlm_client_target_domain_dns(ntlm_client *ntlm);

/**
 * Computes a negotiation message (aka a "Type 1" message) to begin
 * NTLM authentication with the server.  The local hostname should be
 * set before calling this function (if necessary).  This message
 * should be delivered to the server to indicate a willingness to begin
 * NTLM authentication.  This buffer should not be freed by the caller.
 *
 * @param out a pointer to the negotiation message
 * @param out_len a pointer to the length of the negotiation message
 * @param ntlm the `ntlm_client` context
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_negotiate(
	const unsigned char **out,
	size_t *out_len,
	ntlm_client *ntlm);

/**
 * Parses a challenge message (aka a "Type 2" message) from the server.
 * This must be called in order to calculate the response to the
 * authentication.
 *
 * @param ntlm the `ntlm_client` context
 * @param message the challenge message from the server
 * @param message_len the length of the challenge message
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_set_challenge(
	ntlm_client *ntlm,
	const unsigned char *message,
	size_t message_len);

/**
 * Computes a response message (aka a "Type 3" message) to complete
 * NTLM authentication with the server.  The credentials should be
 * set before calling this function.  This message should be delivered
 * to the server to complete authentication.  This buffer should not
 * be freed by the caller.
 *
 * @param out a pointer to the response message
 * @param out_len a pointer to the length of the response message
 * @param ntlm the `ntlm_client` context
 * @return 0 on success, non-zero on failure
 */
NTLM_EXTERN(int) ntlm_client_response(
	const unsigned char **out,
	size_t *out_len,
	ntlm_client *ntlm);

/**
 * Resets an `ntlm_client` context completely, so that authentication
 * may be retried.  You must set _all_ parameters again, including the
 * target, username, password, etc.  Once these values are configured
 * again, the negotiation can begin.
 *
 * @param ntlm the `ntlm_client` context to reset
 */
NTLM_EXTERN(void) ntlm_client_reset(ntlm_client *ntlm);

/**
 * Frees an `ntlm_client` context.  This should be done to free memory
 * belonging to the context.  The context cannot be reused.
 *
 * @param ntlm the `ntlm_client` context to free
 */
NTLM_EXTERN(void) ntlm_client_free(ntlm_client *ntlm);

#ifdef __cplusplus
}
#endif

#endif /* INCLUDE_NTLMCLIENT_H__ */
