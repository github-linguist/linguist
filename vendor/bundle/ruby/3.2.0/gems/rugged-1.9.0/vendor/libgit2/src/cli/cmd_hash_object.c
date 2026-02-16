/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <git2.h>
#include "common.h"
#include "cmd.h"

#include "futils.h"

#define COMMAND_NAME "hash-object"

static char *type_name;
static int write_object, read_stdin, literally;
static char **filenames;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT,

	{ CLI_OPT_TYPE_VALUE,     NULL,      't', &type_name,    0,
	  CLI_OPT_USAGE_DEFAULT, "type",     "the type of object to hash (default: \"blob\")" },
	{ CLI_OPT_TYPE_SWITCH,    NULL,      'w', &write_object, 1,
	  CLI_OPT_USAGE_DEFAULT,  NULL,      "write the object to the object database" },
	{ CLI_OPT_TYPE_SWITCH,   "literally", 0, &literally,    1,
	  CLI_OPT_USAGE_DEFAULT,  NULL,      "do not validate the object contents" },
	{ CLI_OPT_TYPE_SWITCH,   "stdin",     0, &read_stdin,   1,
	  CLI_OPT_USAGE_REQUIRED, NULL,      "read content from stdin" },
	{ CLI_OPT_TYPE_ARGS,     "file",      0, &filenames,    0,
	  CLI_OPT_USAGE_CHOICE,  "file",     "the file (or files) to read and hash" },
	{ 0 },
};

static void print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, 0);
	printf("\n");

	printf("Compute the object ID for a given file and optionally write that file\nto the object database.\n");
	printf("\n");

	printf("Options:\n");

	cli_opt_help_fprint(stdout, opts);
}

static int hash_buf(
	git_odb *odb,
	git_str *buf,
	git_object_t object_type,
	git_oid_t oid_type)
{
	git_oid oid;

	if (!literally) {
		int valid = 0;

#ifdef GIT_EXPERIMENTAL_SHA256
		if (git_object_rawcontent_is_valid(&valid, buf->ptr, buf->size, object_type, oid_type) < 0 || !valid)
			return cli_error_git();
#else
		GIT_UNUSED(oid_type);

		if (git_object_rawcontent_is_valid(&valid, buf->ptr, buf->size, object_type) < 0 || !valid)
			return cli_error_git();
#endif
	}

	if (write_object) {
		if (git_odb_write(&oid, odb, buf->ptr, buf->size, object_type) < 0)
			return cli_error_git();
	} else {
#ifdef GIT_EXPERIMENTAL_SHA256
		if (git_odb_hash(&oid, buf->ptr, buf->size, object_type, GIT_OID_SHA1) < 0)
			return cli_error_git();
#else
		if (git_odb_hash(&oid, buf->ptr, buf->size, object_type) < 0)
			return cli_error_git();
#endif
	}

	if (printf("%s\n", git_oid_tostr_s(&oid)) < 0)
		return cli_error_os();

	return 0;
}

int cmd_hash_object(int argc, char **argv)
{
	cli_repository_open_options open_opts = { argv + 1, argc - 1};
	git_repository *repo = NULL;
	git_odb *odb = NULL;
	git_oid_t oid_type;
	git_str buf = GIT_STR_INIT;
	cli_opt invalid_opt;
	git_object_t object_type = GIT_OBJECT_BLOB;
	char **filename;
	int ret = 0;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	if (cli_opt__show_help) {
		print_help();
		return 0;
	}

	if (type_name && (object_type = git_object_string2type(type_name)) == GIT_OBJECT_INVALID)
		return cli_error_usage("invalid object type '%s'", type_name);

	if (write_object &&
	    (cli_repository_open(&repo, &open_opts) < 0 ||
	     git_repository_odb(&odb, repo) < 0)) {
		ret = cli_error_git();
		goto done;
	}

	oid_type = git_repository_oid_type(repo);

	/*
	 * TODO: we're reading blobs, we shouldn't pull them all into main
	 * memory, we should just stream them into the odb instead.
	 * (Or create a `git_odb_writefile` API.)
	 */
	if (read_stdin) {
		if (git_futils_readbuffer_fd_full(&buf, fileno(stdin)) < 0) {
			ret = cli_error_git();
			goto done;
		}

		if ((ret = hash_buf(odb, &buf, object_type, oid_type)) != 0)
			goto done;
	} else {
		for (filename = filenames; *filename; filename++) {
			if (git_futils_readbuffer(&buf, *filename) < 0) {
				ret = cli_error_git();
				goto done;
			}

			if ((ret = hash_buf(odb, &buf, object_type, oid_type)) != 0)
				goto done;
		}
	}

done:
	git_str_dispose(&buf);
	git_odb_free(odb);
	git_repository_free(repo);
	return ret;
}
