/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "grafts.h"

#include "futils.h"
#include "oid.h"
#include "oidarray.h"
#include "parse.h"
#include "hashmap_oid.h"

GIT_HASHMAP_OID_SETUP(git_grafts_oidmap, git_commit_graft *);

struct git_grafts {
	/* Map of `git_commit_graft`s */
	git_grafts_oidmap commits;

	/* Type of object IDs */
	git_oid_t oid_type;

	/* File backing the graft. NULL if it's an in-memory graft */
	char *path;
	unsigned char path_checksum[GIT_HASH_SHA256_SIZE];
};

int git_grafts_new(git_grafts **out, git_oid_t oid_type)
{
	git_grafts *grafts;

	GIT_ASSERT_ARG(out && oid_type);

	grafts = git__calloc(1, sizeof(*grafts));
	GIT_ERROR_CHECK_ALLOC(grafts);

	grafts->oid_type = oid_type;

	*out = grafts;
	return 0;
}

int git_grafts_open(
	git_grafts **out,
	const char *path,
	git_oid_t oid_type)
{
	git_grafts *grafts = NULL;
	int error;

	GIT_ASSERT_ARG(out && path && oid_type);

	if ((error = git_grafts_new(&grafts, oid_type)) < 0)
		goto error;

	grafts->path = git__strdup(path);
	GIT_ERROR_CHECK_ALLOC(grafts->path);

	if ((error = git_grafts_refresh(grafts)) < 0)
		goto error;

	*out = grafts;

error:
	if (error < 0)
		git_grafts_free(grafts);

	return error;
}

int git_grafts_open_or_refresh(
	git_grafts **out,
	const char *path,
	git_oid_t oid_type)
{
	GIT_ASSERT_ARG(out && path && oid_type);

	return *out ? git_grafts_refresh(*out) : git_grafts_open(out, path, oid_type);
}

void git_grafts_free(git_grafts *grafts)
{
	if (!grafts)
		return;
	git__free(grafts->path);
	git_grafts_clear(grafts);
	git_grafts_oidmap_dispose(&grafts->commits);
	git__free(grafts);
}

void git_grafts_clear(git_grafts *grafts)
{
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;
	git_commit_graft *graft;

	if (!grafts)
		return;

	while (git_grafts_oidmap_iterate(&iter, NULL, &graft, &grafts->commits) == 0) {
		git__free(graft->parents.ptr);
		git__free(graft);
	}

	git_grafts_oidmap_clear(&grafts->commits);
}

int git_grafts_refresh(git_grafts *grafts)
{
	git_str contents = GIT_STR_INIT;
	int error, updated = 0;

	GIT_ASSERT_ARG(grafts);

	if (!grafts->path)
		return 0;

	if ((error = git_futils_readbuffer_updated(&contents, grafts->path,
				grafts->path_checksum, &updated)) < 0) {

		if (error == GIT_ENOTFOUND) {
			git_grafts_clear(grafts);
			error = 0;
		}

		goto cleanup;
	}

	if (!updated) {
		goto cleanup;
	}

	if ((error = git_grafts_parse(grafts, contents.ptr, contents.size)) < 0)
		goto cleanup;

cleanup:
	git_str_dispose(&contents);
	return error;
}

int git_grafts_parse(git_grafts *grafts, const char *buf, size_t len)
{
	git_array_oid_t parents = GIT_ARRAY_INIT;
	git_parse_ctx parser;
	int error;

	git_grafts_clear(grafts);

	if ((error = git_parse_ctx_init(&parser, buf, len)) < 0)
		goto error;

	for (; parser.remain_len; git_parse_advance_line(&parser)) {
		git_oid graft_oid;

		if ((error = git_parse_advance_oid(&graft_oid, &parser, grafts->oid_type)) < 0) {
			git_error_set(GIT_ERROR_GRAFTS, "invalid graft OID at line %" PRIuZ, parser.line_num);
			goto error;
		}

		while (parser.line_len && git_parse_advance_expected(&parser, "\n", 1) != 0) {
			git_oid *id = git_array_alloc(parents);
			GIT_ERROR_CHECK_ALLOC(id);

			if ((error = git_parse_advance_expected(&parser, " ", 1)) < 0 ||
			    (error = git_parse_advance_oid(id, &parser, grafts->oid_type)) < 0) {
				git_error_set(GIT_ERROR_GRAFTS, "invalid parent OID at line %" PRIuZ, parser.line_num);
				goto error;
			}
		}

		if ((error = git_grafts_add(grafts, &graft_oid, parents)) < 0)
			goto error;

		git_array_clear(parents);
	}

error:
	git_array_clear(parents);
	return error;
}

int git_grafts_add(git_grafts *grafts, const git_oid *oid, git_array_oid_t parents)
{
	git_commit_graft *graft;
	git_oid *parent_oid;
	int error;
	size_t i;

	GIT_ASSERT_ARG(grafts && oid);

	graft = git__calloc(1, sizeof(*graft));
	GIT_ERROR_CHECK_ALLOC(graft);

	git_array_init_to_size(graft->parents, git_array_size(parents));
	git_array_foreach(parents, i, parent_oid) {
		git_oid *id = git_array_alloc(graft->parents);
		GIT_ERROR_CHECK_ALLOC(id);

		git_oid_cpy(id, parent_oid);
	}
	git_oid_cpy(&graft->oid, oid);

	if ((error = git_grafts_remove(grafts, &graft->oid)) < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if ((error = git_grafts_oidmap_put(&grafts->commits, &graft->oid, graft)) < 0)
		goto cleanup;

	return 0;

cleanup:
	git_array_clear(graft->parents);
	git__free(graft);
	return error;
}

int git_grafts_remove(git_grafts *grafts, const git_oid *oid)
{
	git_commit_graft *graft;
	int error;

	GIT_ASSERT_ARG(grafts && oid);

	if (git_grafts_oidmap_get(&graft, &grafts->commits, oid) != 0)
		return GIT_ENOTFOUND;

	if ((error = git_grafts_oidmap_remove(&grafts->commits, oid)) < 0)
		return error;

	git__free(graft->parents.ptr);
	git__free(graft);

	return 0;
}

int git_grafts_get(git_commit_graft **out, git_grafts *grafts, const git_oid *oid)
{
	GIT_ASSERT_ARG(out && grafts && oid);
	return git_grafts_oidmap_get(out, &grafts->commits, oid);
}

int git_grafts_oids(git_oid **out, size_t *out_len, git_grafts *grafts)
{
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;
	git_array_oid_t array = GIT_ARRAY_INIT;
	const git_oid *oid;
	size_t existing;

	GIT_ASSERT_ARG(out && grafts);

	if ((existing = git_grafts_oidmap_size(&grafts->commits)) > 0)
		git_array_init_to_size(array, existing);

	while (git_grafts_oidmap_iterate(&iter, &oid, NULL, &grafts->commits) == 0) {
		git_oid *cpy = git_array_alloc(array);
		GIT_ERROR_CHECK_ALLOC(cpy);
		git_oid_cpy(cpy, oid);
	}

	*out = array.ptr;
	*out_len = array.size;

	return 0;
}

size_t git_grafts_size(git_grafts *grafts)
{
	return git_grafts_oidmap_size(&grafts->commits);
}
