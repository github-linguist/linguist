/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "futils.h"
#include "repository.h"
#include "config.h"
#include "git2/config.h"
#include "vector.h"
#include "filter.h"

struct map_data {
	const char *name;
	git_configmap *maps;
	size_t map_count;
	int default_value;
};

/*
 *	core.eol
 *		Sets the line ending type to use in the working directory for
 *	files that have the text property set. Alternatives are lf, crlf
 *	and native, which uses the platform's native line ending. The default
 *	value is native. See gitattributes(5) for more information on
 *	end-of-line conversion.
 */
static git_configmap _configmap_eol[] = {
	{GIT_CONFIGMAP_FALSE, NULL, GIT_EOL_UNSET},
	{GIT_CONFIGMAP_STRING, "lf", GIT_EOL_LF},
	{GIT_CONFIGMAP_STRING, "crlf", GIT_EOL_CRLF},
	{GIT_CONFIGMAP_STRING, "native", GIT_EOL_NATIVE}
};

/*
 *	core.autocrlf
 *		Setting this variable to "true" is almost the same as setting
 *	the text attribute to "auto" on all files except that text files are
 *	not guaranteed to be normalized: files that contain CRLF in the
 *	repository will not be touched. Use this setting if you want to have
 *	CRLF line endings in your working directory even though the repository
 *	does not have normalized line endings. This variable can be set to input,
 *	in which case no output conversion is performed.
 */
static git_configmap _configmap_autocrlf[] = {
	{GIT_CONFIGMAP_FALSE, NULL, GIT_AUTO_CRLF_FALSE},
	{GIT_CONFIGMAP_TRUE, NULL, GIT_AUTO_CRLF_TRUE},
	{GIT_CONFIGMAP_STRING, "input", GIT_AUTO_CRLF_INPUT}
};

static git_configmap _configmap_safecrlf[] = {
	{GIT_CONFIGMAP_FALSE, NULL, GIT_SAFE_CRLF_FALSE},
	{GIT_CONFIGMAP_TRUE, NULL, GIT_SAFE_CRLF_FAIL},
	{GIT_CONFIGMAP_STRING, "warn", GIT_SAFE_CRLF_WARN}
};

static git_configmap _configmap_logallrefupdates[] = {
	{GIT_CONFIGMAP_FALSE, NULL, GIT_LOGALLREFUPDATES_FALSE},
	{GIT_CONFIGMAP_TRUE, NULL, GIT_LOGALLREFUPDATES_TRUE},
	{GIT_CONFIGMAP_STRING, "always", GIT_LOGALLREFUPDATES_ALWAYS},
};

static git_configmap _configmap_abbrev[] = {
	{GIT_CONFIGMAP_INT32, NULL, 0},
	{GIT_CONFIGMAP_FALSE, NULL, GIT_ABBREV_FALSE},
	{GIT_CONFIGMAP_STRING, "auto", GIT_ABBREV_DEFAULT}
};

static struct map_data _configmaps[] = {
	{"core.autocrlf", _configmap_autocrlf, ARRAY_SIZE(_configmap_autocrlf), GIT_AUTO_CRLF_DEFAULT},
	{"core.eol", _configmap_eol, ARRAY_SIZE(_configmap_eol), GIT_EOL_DEFAULT},
	{"core.symlinks", NULL, 0, GIT_SYMLINKS_DEFAULT },
	{"core.ignorecase", NULL, 0, GIT_IGNORECASE_DEFAULT },
	{"core.filemode", NULL, 0, GIT_FILEMODE_DEFAULT },
	{"core.ignorestat", NULL, 0, GIT_IGNORESTAT_DEFAULT },
	{"core.trustctime", NULL, 0, GIT_TRUSTCTIME_DEFAULT },
	{"core.abbrev", _configmap_abbrev, ARRAY_SIZE(_configmap_abbrev), GIT_ABBREV_DEFAULT },
	{"core.precomposeunicode", NULL, 0, GIT_PRECOMPOSE_DEFAULT },
	{"core.safecrlf", _configmap_safecrlf, ARRAY_SIZE(_configmap_safecrlf), GIT_SAFE_CRLF_DEFAULT},
	{"core.logallrefupdates", _configmap_logallrefupdates, ARRAY_SIZE(_configmap_logallrefupdates), GIT_LOGALLREFUPDATES_DEFAULT},
	{"core.protecthfs", NULL, 0, GIT_PROTECTHFS_DEFAULT },
	{"core.protectntfs", NULL, 0, GIT_PROTECTNTFS_DEFAULT },
	{"core.fsyncobjectfiles", NULL, 0, GIT_FSYNCOBJECTFILES_DEFAULT },
	{"core.longpaths", NULL, 0, GIT_LONGPATHS_DEFAULT },
};

int git_config__configmap_lookup(int *out, git_config *config, git_configmap_item item)
{
	int error = 0;
	struct map_data *data = &_configmaps[(int)item];
	git_config_entry *entry;

	if ((error = git_config__lookup_entry(&entry, config, data->name, false)) < 0)
		return error;

	if (!entry)
		*out = data->default_value;
	else if (data->maps)
		error = git_config_lookup_map_value(
			out, data->maps, data->map_count, entry->value);
	else
		error = git_config_parse_bool(out, entry->value);

	git_config_entry_free(entry);
	return error;
}

int git_repository__configmap_lookup(int *out, git_repository *repo, git_configmap_item item)
{
	intptr_t value = (intptr_t)git_atomic_load(repo->configmap_cache[(int)item]);

	*out = (int)value;

	if (value == GIT_CONFIGMAP_NOT_CACHED) {
		git_config *config;
		intptr_t oldval = value;
		int error;

		if ((error = git_repository_config__weakptr(&config, repo)) < 0 ||
			(error = git_config__configmap_lookup(out, config, item)) < 0)
			return error;

		value = *out;
		git_atomic_compare_and_swap(&repo->configmap_cache[(int)item], (void *)oldval, (void *)value);
	}

	return 0;
}

void git_repository__configmap_lookup_cache_clear(git_repository *repo)
{
	int i;

	for (i = 0; i < GIT_CONFIGMAP_CACHE_MAX; ++i)
		repo->configmap_cache[i] = GIT_CONFIGMAP_NOT_CACHED;
}

