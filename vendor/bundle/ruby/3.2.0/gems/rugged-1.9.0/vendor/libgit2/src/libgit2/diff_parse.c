/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "diff_parse.h"

#include "diff.h"
#include "patch.h"
#include "patch_parse.h"

static void diff_parsed_free(git_diff *d)
{
	git_diff_parsed *diff = (git_diff_parsed *)d;
	git_patch *patch;
	size_t i;

	git_vector_foreach(&diff->patches, i, patch)
		git_patch_free(patch);

	git_vector_dispose(&diff->patches);

	git_vector_dispose(&diff->base.deltas);
	git_pool_clear(&diff->base.pool);

	git__memzero(diff, sizeof(*diff));
	git__free(diff);
}

static git_diff_parsed *diff_parsed_alloc(git_oid_t oid_type)
{
	git_diff_parsed *diff;

	if ((diff = git__calloc(1, sizeof(git_diff_parsed))) == NULL)
		return NULL;

	GIT_REFCOUNT_INC(&diff->base);
	diff->base.type = GIT_DIFF_TYPE_PARSED;
	diff->base.strcomp = git__strcmp;
	diff->base.strncomp = git__strncmp;
	diff->base.pfxcomp = git__prefixcmp;
	diff->base.entrycomp = git_diff__entry_cmp;
	diff->base.patch_fn = git_patch_parsed_from_diff;
	diff->base.free_fn = diff_parsed_free;

	if (git_diff_options_init(&diff->base.opts, GIT_DIFF_OPTIONS_VERSION) < 0) {
		git__free(diff);
		return NULL;
	}

	diff->base.opts.flags &= ~GIT_DIFF_IGNORE_CASE;
	diff->base.opts.oid_type = oid_type;

	if (git_pool_init(&diff->base.pool, 1) < 0 ||
	    git_vector_init(&diff->patches, 0, NULL) < 0 ||
		git_vector_init(&diff->base.deltas, 0, git_diff_delta__cmp) < 0) {
		git_diff_free(&diff->base);
		return NULL;
	}

	git_vector_set_cmp(&diff->base.deltas, git_diff_delta__cmp);

	return diff;
}

int git_diff_from_buffer(
	git_diff **out,
	const char *content,
	size_t content_len
#ifdef GIT_EXPERIMENTAL_SHA256
	, git_diff_parse_options *opts
#endif
	)
{
	git_diff_parsed *diff;
	git_patch *patch;
	git_patch_parse_ctx *ctx = NULL;
	git_patch_options patch_opts = GIT_PATCH_OPTIONS_INIT;
	git_oid_t oid_type;
	int error = 0;

	*out = NULL;

#ifdef GIT_EXPERIMENTAL_SHA256
	oid_type = (opts && opts->oid_type) ? opts->oid_type :
		GIT_OID_DEFAULT;
#else
	oid_type = GIT_OID_DEFAULT;
#endif

	patch_opts.oid_type = oid_type;

	diff = diff_parsed_alloc(oid_type);
	GIT_ERROR_CHECK_ALLOC(diff);

	ctx = git_patch_parse_ctx_init(content, content_len, &patch_opts);
	GIT_ERROR_CHECK_ALLOC(ctx);

	while (ctx->parse_ctx.remain_len) {
		if ((error = git_patch_parse(&patch, ctx)) < 0)
			break;

		git_vector_insert(&diff->patches, patch);
		git_vector_insert(&diff->base.deltas, patch->delta);
	}

	if (error == GIT_ENOTFOUND && git_vector_length(&diff->patches) > 0) {
		git_error_clear();
		error = 0;
	}

	git_patch_parse_ctx_free(ctx);

	if (error < 0)
		git_diff_free(&diff->base);
	else
		*out = &diff->base;

	return error;
}

