/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "diff_driver.h"

#include "git2/attr.h"

#include "common.h"
#include "diff.h"
#include "map.h"
#include "config.h"
#include "regexp.h"
#include "repository.h"
#include "userdiff.h"

typedef enum {
	DIFF_DRIVER_AUTO = 0,
	DIFF_DRIVER_BINARY = 1,
	DIFF_DRIVER_TEXT = 2,
	DIFF_DRIVER_PATTERNLIST = 3
} git_diff_driver_t;

typedef struct {
	git_regexp re;
	int flags;
} git_diff_driver_pattern;

enum {
	REG_NEGATE = (1 << 15) /* get out of the way of existing flags */
};

/* data for finding function context for a given file type */
struct git_diff_driver {
	git_diff_driver_t type;
	uint32_t binary_flags;
	uint32_t other_flags;
	git_array_t(git_diff_driver_pattern) fn_patterns;
	git_regexp  word_pattern;
	char name[GIT_FLEX_ARRAY];
};

GIT_HASHMAP_STR_SETUP(git_diff_driver_map, git_diff_driver *);

struct git_diff_driver_registry {
	git_diff_driver_map map;
};

#define FORCE_DIFFABLE (GIT_DIFF_FORCE_TEXT | GIT_DIFF_FORCE_BINARY)

static git_diff_driver diff_driver_auto =   { DIFF_DRIVER_AUTO,   0, 0 };
static git_diff_driver diff_driver_binary = { DIFF_DRIVER_BINARY, GIT_DIFF_FORCE_BINARY, 0 };
static git_diff_driver diff_driver_text =   { DIFF_DRIVER_TEXT,   GIT_DIFF_FORCE_TEXT, 0 };

git_diff_driver_registry *git_diff_driver_registry_new(void)
{
	return git__calloc(1, sizeof(git_diff_driver_registry));
}

void git_diff_driver_registry_free(git_diff_driver_registry *reg)
{
	git_diff_driver *drv;
	git_hashmap_iter_t iter = 0;

	if (!reg)
		return;

	while (git_diff_driver_map_iterate(&iter, NULL, &drv, &reg->map) == 0)
		git_diff_driver_free(drv);

	git_diff_driver_map_dispose(&reg->map);
	git__free(reg);
}

static int diff_driver_add_patterns(
	git_diff_driver *drv, const char *regex_str, int regex_flags)
{
	int error = 0;
	const char *scan, *end;
	git_diff_driver_pattern *pat = NULL;
	git_str buf = GIT_STR_INIT;

	for (scan = regex_str; scan; scan = end) {
		/* get pattern to fill in */
		if ((pat = git_array_alloc(drv->fn_patterns)) == NULL) {
			return -1;
		}

		pat->flags = regex_flags;
		if (*scan == '!') {
			pat->flags |= REG_NEGATE;
			++scan;
		}

		if ((end = strchr(scan, '\n')) != NULL) {
			error = git_str_set(&buf, scan, end - scan);
			end++;
		} else {
			error = git_str_sets(&buf, scan);
		}
		if (error < 0)
			break;

		if ((error = git_regexp_compile(&pat->re, buf.ptr, regex_flags)) != 0) {
			/*
			 * TODO: issue a warning
			 */
		}
	}

	if (error && pat != NULL)
		(void)git_array_pop(drv->fn_patterns); /* release last item */
	git_str_dispose(&buf);

	/* We want to ignore bad patterns, so return success regardless */
	return 0;
}

static int diff_driver_xfuncname(const git_config_entry *entry, void *payload)
{
	return diff_driver_add_patterns(payload, entry->value, 0);
}

static int diff_driver_funcname(const git_config_entry *entry, void *payload)
{
	return diff_driver_add_patterns(payload, entry->value, 0);
}

static git_diff_driver_registry *git_repository_driver_registry(
	git_repository *repo)
{
	git_diff_driver_registry *reg = git_atomic_load(repo->diff_drivers), *newreg;
	if (reg)
		return reg;

	newreg = git_diff_driver_registry_new();
	if (!newreg) {
		git_error_set(GIT_ERROR_REPOSITORY, "unable to create diff driver registry");
		return newreg;
	}
	reg = git_atomic_compare_and_swap(&repo->diff_drivers, NULL, newreg);
	if (!reg) {
		reg = newreg;
	} else {
		/* if we race, free losing allocation */
		git_diff_driver_registry_free(newreg);
	}
	return reg;
}

static int diff_driver_alloc(
	git_diff_driver **out, size_t *namelen_out, const char *name)
{
	git_diff_driver *driver;
	size_t driverlen = sizeof(git_diff_driver),
		namelen = strlen(name),
		alloclen;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, driverlen, namelen);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);

	driver = git__calloc(1, alloclen);
	GIT_ERROR_CHECK_ALLOC(driver);

	memcpy(driver->name, name, namelen);

	*out = driver;

	if (namelen_out)
		*namelen_out = namelen;

	return 0;
}

static int git_diff_driver_builtin(
	git_diff_driver **out,
	git_diff_driver_registry *reg,
	const char *driver_name)
{
	git_diff_driver_definition *ddef = NULL;
	git_diff_driver *drv = NULL;
	int error = 0;
	size_t idx;

	for (idx = 0; idx < ARRAY_SIZE(builtin_defs); ++idx) {
		if (!strcasecmp(driver_name, builtin_defs[idx].name)) {
			ddef = &builtin_defs[idx];
			break;
		}
	}
	if (!ddef)
		goto done;

	if ((error = diff_driver_alloc(&drv, NULL, ddef->name)) < 0)
		goto done;

	drv->type = DIFF_DRIVER_PATTERNLIST;

	if (ddef->fns &&
		(error = diff_driver_add_patterns(
			drv, ddef->fns, ddef->flags)) < 0)
		goto done;

	if (ddef->words &&
	    (error = git_regexp_compile(&drv->word_pattern, ddef->words, ddef->flags)) < 0)
		goto done;

	if ((error = git_diff_driver_map_put(&reg->map, drv->name, drv)) < 0)
		goto done;

done:
	if (error && drv)
		git_diff_driver_free(drv);
	else
		*out = drv;

	return error;
}

static int git_diff_driver_load(
	git_diff_driver **out, git_repository *repo, const char *driver_name)
{
	int error = 0;
	git_diff_driver_registry *reg;
	git_diff_driver *drv;
	size_t namelen;
	git_config *cfg = NULL;
	git_str name = GIT_STR_INIT;
	git_config_entry *ce = NULL;
	bool found_driver = false;

	if ((reg = git_repository_driver_registry(repo)) == NULL)
		return -1;

	if (git_diff_driver_map_get(&drv, &reg->map, driver_name) == 0) {
		*out = drv;
		return 0;
	}

	if ((error = diff_driver_alloc(&drv, &namelen, driver_name)) < 0)
		goto done;

	drv->type = DIFF_DRIVER_AUTO;

	/* if you can't read config for repo, just use default driver */
	if (git_repository_config_snapshot(&cfg, repo) < 0) {
		git_error_clear();
		goto done;
	}

	if ((error = git_str_printf(&name, "diff.%s.binary", driver_name)) < 0)
		goto done;

	switch (git_config__get_bool_force(cfg, name.ptr, -1)) {
	case true:
		/* if diff.<driver>.binary is true, just return the binary driver */
		*out = &diff_driver_binary;
		goto done;
	case false:
		/* if diff.<driver>.binary is false, force binary checks off */
		/* but still may have custom function context patterns, etc. */
		drv->binary_flags = GIT_DIFF_FORCE_TEXT;
		found_driver = true;
		break;
	default:
		/* diff.<driver>.binary unspecified or "auto", so just continue */
		break;
	}

	/* TODO: warn if diff.<name>.command or diff.<name>.textconv are set */

	git_str_truncate(&name, namelen + strlen("diff.."));
	if ((error = git_str_PUTS(&name, "xfuncname")) < 0)
		goto done;

	if ((error = git_config_get_multivar_foreach(
			cfg, name.ptr, NULL, diff_driver_xfuncname, drv)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto done;
		git_error_clear(); /* no diff.<driver>.xfuncname, so just continue */
	}

	git_str_truncate(&name, namelen + strlen("diff.."));
	if ((error = git_str_PUTS(&name, "funcname")) < 0)
		goto done;

	if ((error = git_config_get_multivar_foreach(
			cfg, name.ptr, NULL, diff_driver_funcname, drv)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto done;
		git_error_clear(); /* no diff.<driver>.funcname, so just continue */
	}

	/* if we found any patterns, set driver type to use correct callback */
	if (git_array_size(drv->fn_patterns) > 0) {
		drv->type = DIFF_DRIVER_PATTERNLIST;
		found_driver = true;
	}

	git_str_truncate(&name, namelen + strlen("diff.."));
	if ((error = git_str_PUTS(&name, "wordregex")) < 0)
		goto done;

	if ((error = git_config__lookup_entry(&ce, cfg, name.ptr, false)) < 0)
		goto done;
	if (!ce || !ce->value)
		/* no diff.<driver>.wordregex, so just continue */;
	else if (!(error = git_regexp_compile(&drv->word_pattern, ce->value, 0)))
		found_driver = true;
	else {
		/* TODO: warn about bad regex instead of failure */
		goto done;
	}

	/* TODO: look up diff.<driver>.algorithm to turn on minimal / patience
	 * diff in drv->other_flags
	 */

	/* if no driver config found at all, fall back on AUTO driver */
	if (!found_driver)
		goto done;

	/* store driver in registry */
	if ((error = git_diff_driver_map_put(&reg->map, drv->name, drv)) < 0)
		goto done;

	*out = drv;

done:
	git_config_entry_free(ce);
	git_str_dispose(&name);
	git_config_free(cfg);

	if (!*out) {
		int error2 = git_diff_driver_builtin(out, reg, driver_name);
		if (!error)
			error = error2;
	}

	if (drv && drv != *out)
		git_diff_driver_free(drv);

	return error;
}

int git_diff_driver_lookup(
	git_diff_driver **out, git_repository *repo,
	git_attr_session *attrsession, const char *path)
{
	int error = 0;
	const char *values[1], *attrs[] = { "diff" };

	GIT_ASSERT_ARG(out);
	*out = NULL;

	if (!repo || !path || !strlen(path))
		/* just use the auto value */;
	else if ((error = git_attr_get_many_with_session(values, repo,
			attrsession, 0, path, 1, attrs)) < 0)
		/* return error below */;

	else if (GIT_ATTR_IS_UNSPECIFIED(values[0]))
		/* just use the auto value */;
	else if (GIT_ATTR_IS_FALSE(values[0]))
		*out = &diff_driver_binary;
	else if (GIT_ATTR_IS_TRUE(values[0]))
		*out = &diff_driver_text;

	/* otherwise look for driver information in config and build driver */
	else if ((error = git_diff_driver_load(out, repo, values[0])) < 0) {
		if (error == GIT_ENOTFOUND) {
			error = 0;
			git_error_clear();
		}
	}

	if (!*out)
		*out = &diff_driver_auto;

	return error;
}

void git_diff_driver_free(git_diff_driver *driver)
{
	git_diff_driver_pattern *pat;

	if (!driver)
		return;

	while ((pat = git_array_pop(driver->fn_patterns)) != NULL)
		git_regexp_dispose(&pat->re);
	git_array_clear(driver->fn_patterns);

	git_regexp_dispose(&driver->word_pattern);

	git__free(driver);
}

void git_diff_driver_update_options(
	uint32_t *option_flags, git_diff_driver *driver)
{
	if ((*option_flags & FORCE_DIFFABLE) == 0)
		*option_flags |= driver->binary_flags;

	*option_flags |= driver->other_flags;
}

int git_diff_driver_content_is_binary(
	git_diff_driver *driver, const char *content, size_t content_len)
{
	git_str search = GIT_STR_INIT;

	GIT_UNUSED(driver);

	git_str_attach_notowned(&search, content,
		min(content_len, GIT_FILTER_BYTES_TO_CHECK_NUL));

	/* TODO: provide encoding / binary detection callbacks that can
	 * be UTF-8 aware, etc.  For now, instead of trying to be smart,
	 * let's just use the simple NUL-byte detection that core git uses.
	 */

	/* previously was: if (git_str_is_binary(&search)) */
	if (git_str_contains_nul(&search))
		return 1;

	return 0;
}

static int diff_context_line__simple(
	git_diff_driver *driver, git_str *line)
{
	char firstch = line->ptr[0];
	GIT_UNUSED(driver);
	return (git__isalpha(firstch) || firstch == '_' || firstch == '$');
}

static int diff_context_line__pattern_match(
	git_diff_driver *driver, git_str *line)
{
	size_t i, maxi = git_array_size(driver->fn_patterns);
	git_regmatch pmatch[2];

	for (i = 0; i < maxi; ++i) {
		git_diff_driver_pattern *pat = git_array_get(driver->fn_patterns, i);

		if (!git_regexp_search(&pat->re, line->ptr, 2, pmatch)) {
			if (pat->flags & REG_NEGATE)
				return false;

			/* use pmatch data to trim line data */
			i = (pmatch[1].start >= 0) ? 1 : 0;
			git_str_consume(line, git_str_cstr(line) + pmatch[i].start);
			git_str_truncate(line, pmatch[i].end - pmatch[i].start);
			git_str_rtrim(line);

			return true;
		}
	}

	return false;
}

static long diff_context_find(
	const char *line,
	long line_len,
	char *out,
	long out_size,
	void *payload)
{
	git_diff_find_context_payload *ctxt = payload;

	if (git_str_set(&ctxt->line, line, (size_t)line_len) < 0)
		return -1;
	git_str_rtrim(&ctxt->line);

	if (!ctxt->line.size)
		return -1;

	if (!ctxt->match_line || !ctxt->match_line(ctxt->driver, &ctxt->line))
		return -1;

	if (out_size > (long)ctxt->line.size)
		out_size = (long)ctxt->line.size;
	memcpy(out, ctxt->line.ptr, (size_t)out_size);

	return out_size;
}

void git_diff_find_context_init(
	git_diff_find_context_fn *findfn_out,
	git_diff_find_context_payload *payload_out,
	git_diff_driver *driver)
{
	*findfn_out = driver ? diff_context_find : NULL;

	memset(payload_out, 0, sizeof(*payload_out));
	if (driver) {
		payload_out->driver = driver;
		payload_out->match_line = (driver->type == DIFF_DRIVER_PATTERNLIST) ?
			diff_context_line__pattern_match : diff_context_line__simple;
		git_str_init(&payload_out->line, 0);
	}
}

void git_diff_find_context_clear(git_diff_find_context_payload *payload)
{
	if (payload) {
		git_str_dispose(&payload->line);
		payload->driver = NULL;
	}
}
