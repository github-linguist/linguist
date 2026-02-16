/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "fetchhead.h"

#include "git2/types.h"
#include "git2/oid.h"

#include "str.h"
#include "futils.h"
#include "filebuf.h"
#include "refs.h"
#include "net.h"
#include "repository.h"

int git_fetchhead_ref_cmp(const void *a, const void *b)
{
	const git_fetchhead_ref *one = (const git_fetchhead_ref *)a;
	const git_fetchhead_ref *two = (const git_fetchhead_ref *)b;

	if (one->is_merge && !two->is_merge)
		return -1;
	if (two->is_merge && !one->is_merge)
		return 1;

	if (one->ref_name && two->ref_name)
		return strcmp(one->ref_name, two->ref_name);
	else if (one->ref_name)
		return -1;
	else if (two->ref_name)
		return 1;

	return 0;
}

static char *sanitized_remote_url(const char *remote_url)
{
	git_net_url url = GIT_NET_URL_INIT;
	char *sanitized = NULL;
	int error;

	if (git_net_url_parse(&url, remote_url) == 0) {
		git_str buf = GIT_STR_INIT;

		git__free(url.username);
		git__free(url.password);
		url.username = url.password = NULL;

		if ((error = git_net_url_fmt(&buf, &url)) < 0)
			goto fallback;

		sanitized = git_str_detach(&buf);
	}

fallback:
	if (!sanitized)
		sanitized = git__strdup(remote_url);

	git_net_url_dispose(&url);
	return sanitized;
}

int git_fetchhead_ref_create(
	git_fetchhead_ref **out,
	git_oid *oid,
	unsigned int is_merge,
	const char *ref_name,
	const char *remote_url)
{
	git_fetchhead_ref *fetchhead_ref;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(oid);

	*out = NULL;

	fetchhead_ref = git__malloc(sizeof(git_fetchhead_ref));
	GIT_ERROR_CHECK_ALLOC(fetchhead_ref);

	memset(fetchhead_ref, 0x0, sizeof(git_fetchhead_ref));

	git_oid_cpy(&fetchhead_ref->oid, oid);
	fetchhead_ref->is_merge = is_merge;

	if (ref_name) {
		fetchhead_ref->ref_name = git__strdup(ref_name);
		GIT_ERROR_CHECK_ALLOC(fetchhead_ref->ref_name);
	}

	if (remote_url) {
		fetchhead_ref->remote_url = sanitized_remote_url(remote_url);
		GIT_ERROR_CHECK_ALLOC(fetchhead_ref->remote_url);
	}

	*out = fetchhead_ref;

	return 0;
}

static int fetchhead_ref_write(
	git_filebuf *file,
	git_fetchhead_ref *fetchhead_ref)
{
	char oid[GIT_OID_MAX_HEXSIZE + 1];
	const char *type, *name;
	int head = 0;

	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(fetchhead_ref);

	git_oid_tostr(oid, GIT_OID_MAX_HEXSIZE + 1, &fetchhead_ref->oid);

	if (git__prefixcmp(fetchhead_ref->ref_name, GIT_REFS_HEADS_DIR) == 0) {
		type = "branch ";
		name = fetchhead_ref->ref_name + strlen(GIT_REFS_HEADS_DIR);
	} else if(git__prefixcmp(fetchhead_ref->ref_name,
		GIT_REFS_TAGS_DIR) == 0) {
		type = "tag ";
		name = fetchhead_ref->ref_name + strlen(GIT_REFS_TAGS_DIR);
	} else if (!git__strcmp(fetchhead_ref->ref_name, GIT_HEAD_FILE)) {
		head = 1;
	} else {
		type = "";
		name = fetchhead_ref->ref_name;
	}

	if (head)
		return git_filebuf_printf(file, "%s\t\t%s\n", oid, fetchhead_ref->remote_url);

	return git_filebuf_printf(file, "%s\t%s\t%s'%s' of %s\n",
		oid,
		(fetchhead_ref->is_merge) ? "" : "not-for-merge",
		type,
		name,
		fetchhead_ref->remote_url);
}

int git_fetchhead_write(git_repository *repo, git_vector *fetchhead_refs)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_str path = GIT_STR_INIT;
	unsigned int i;
	git_fetchhead_ref *fetchhead_ref;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(fetchhead_refs);

	if (git_str_joinpath(&path, repo->gitdir, GIT_FETCH_HEAD_FILE) < 0)
		return -1;

	if (git_filebuf_open(&file, path.ptr, GIT_FILEBUF_APPEND, GIT_REFS_FILE_MODE) < 0) {
		git_str_dispose(&path);
		return -1;
	}

	git_str_dispose(&path);

	git_vector_sort(fetchhead_refs);

	git_vector_foreach(fetchhead_refs, i, fetchhead_ref)
		fetchhead_ref_write(&file, fetchhead_ref);

	return git_filebuf_commit(&file);
}

static int fetchhead_ref_parse(
	git_oid *oid,
	unsigned int *is_merge,
	git_str *ref_name,
	const char **remote_url,
	char *line,
	size_t line_num,
	git_oid_t oid_type)
{
	char *oid_str, *is_merge_str, *desc, *name = NULL;
	const char *type = NULL;
	int error = 0;

	*remote_url = NULL;

	if (!*line) {
		git_error_set(GIT_ERROR_FETCHHEAD,
			"empty line in FETCH_HEAD line %"PRIuZ, line_num);
		return -1;
	}

	/* Compat with old git clients that wrote FETCH_HEAD like a loose ref. */
	if ((oid_str = git__strsep(&line, "\t")) == NULL) {
		oid_str = line;
		line += strlen(line);

		*is_merge = 1;
	}

	if (strlen(oid_str) != git_oid_hexsize(oid_type)) {
		git_error_set(GIT_ERROR_FETCHHEAD,
			"invalid object ID in FETCH_HEAD line %"PRIuZ, line_num);
		return -1;
	}

	if (git_oid__fromstr(oid, oid_str, oid_type) < 0) {
		const git_error *oid_err = git_error_last();
		const char *err_msg = oid_err ? oid_err->message : "invalid object ID";

		git_error_set(GIT_ERROR_FETCHHEAD, "%s in FETCH_HEAD line %"PRIuZ,
			err_msg, line_num);
		return -1;
	}

	/* Parse new data from newer git clients */
	if (*line) {
		if ((is_merge_str = git__strsep(&line, "\t")) == NULL) {
			git_error_set(GIT_ERROR_FETCHHEAD,
				"invalid description data in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if (*is_merge_str == '\0')
			*is_merge = 1;
		else if (strcmp(is_merge_str, "not-for-merge") == 0)
			*is_merge = 0;
		else {
			git_error_set(GIT_ERROR_FETCHHEAD,
				"invalid for-merge entry in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if ((desc = line) == NULL) {
			git_error_set(GIT_ERROR_FETCHHEAD,
				"invalid description in FETCH_HEAD line %"PRIuZ, line_num);
			return -1;
		}

		if (git__prefixcmp(desc, "branch '") == 0) {
			type = GIT_REFS_HEADS_DIR;
			name = desc + 8;
		} else if (git__prefixcmp(desc, "tag '") == 0) {
			type = GIT_REFS_TAGS_DIR;
			name = desc + 5;
		} else if (git__prefixcmp(desc, "'") == 0)
			name = desc + 1;

		if (name) {
			if ((desc = strstr(name, "' ")) == NULL ||
				git__prefixcmp(desc, "' of ") != 0) {
				git_error_set(GIT_ERROR_FETCHHEAD,
					"invalid description in FETCH_HEAD line %"PRIuZ, line_num);
				return -1;
			}

			*desc = '\0';
			desc += 5;
		}

		*remote_url = desc;
	}

	git_str_clear(ref_name);

	if (type)
		git_str_join(ref_name, '/', type, name);
	else if(name)
		git_str_puts(ref_name, name);

	return error;
}

int git_repository_fetchhead_foreach(
	git_repository *repo,
	git_repository_fetchhead_foreach_cb cb,
	void *payload)
{
	git_str path = GIT_STR_INIT, file = GIT_STR_INIT, name = GIT_STR_INIT;
	const char *ref_name;
	git_oid oid;
	const char *remote_url;
	unsigned int is_merge = 0;
	char *buffer, *line;
	size_t line_num = 0;
	int error = 0;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(cb);

	if (git_str_joinpath(&path, repo->gitdir, GIT_FETCH_HEAD_FILE) < 0)
		return -1;

	if ((error = git_futils_readbuffer(&file, git_str_cstr(&path))) < 0)
		goto done;

	buffer = file.ptr;

	while ((line = git__strsep(&buffer, "\n")) != NULL) {
		++line_num;

		if ((error = fetchhead_ref_parse(&oid, &is_merge, &name,
				&remote_url, line, line_num,
				repo->oid_type)) < 0)
			goto done;

		if (git_str_len(&name) > 0)
			ref_name = git_str_cstr(&name);
		else
			ref_name = NULL;

		error = cb(ref_name, remote_url, &oid, is_merge, payload);
		if (error) {
			git_error_set_after_callback(error);
			goto done;
		}
	}

	if (*buffer) {
		git_error_set(GIT_ERROR_FETCHHEAD, "no EOL at line %"PRIuZ, line_num+1);
		error = -1;
		goto done;
	}

done:
	git_str_dispose(&file);
	git_str_dispose(&path);
	git_str_dispose(&name);

	return error;
}

void git_fetchhead_ref_free(git_fetchhead_ref *fetchhead_ref)
{
	if (fetchhead_ref == NULL)
		return;

	git__free(fetchhead_ref->remote_url);
	git__free(fetchhead_ref->ref_name);
	git__free(fetchhead_ref);
}

