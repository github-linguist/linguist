/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <git2.h>

#include "common.h"
#include "cmd.h"

#define COMMAND_NAME "config"

typedef enum {
	ACTION_NONE = 0,
	ACTION_GET,
	ACTION_ADD,
	ACTION_REPLACE_ALL,
	ACTION_LIST
} action_t;

static action_t action = ACTION_NONE;
static int show_origin;
static int show_scope;
static int null_separator;
static int config_level;
static char *config_filename;
static char *name, *value, *value_pattern;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT, \

	{ CLI_OPT_TYPE_SWITCH,    "null",       'z', &null_separator, 1,
	  0,                       NULL,        "use NUL as a separator" },

	{ CLI_OPT_TYPE_SWITCH,    "system",      0,  &config_level, GIT_CONFIG_LEVEL_SYSTEM,
	  0,                       NULL,        "read/write to system configuration" },
	{ CLI_OPT_TYPE_SWITCH,    "global",      0,  &config_level, GIT_CONFIG_LEVEL_GLOBAL,
	  CLI_OPT_USAGE_CHOICE,    NULL,        "read/write to global configuration" },
	{ CLI_OPT_TYPE_SWITCH,    "local",       0,  &config_level, GIT_CONFIG_LEVEL_LOCAL,
	  CLI_OPT_USAGE_CHOICE,    NULL,        "read/write to local configuration" },
	{ CLI_OPT_TYPE_VALUE,     "file",        0,  &config_filename, 0,
	  CLI_OPT_USAGE_CHOICE,   "filename",   "read/write to specified configuration file" },

	{ CLI_OPT_TYPE_SWITCH,    "get",         0,  &action,       ACTION_GET,
	  CLI_OPT_USAGE_REQUIRED,  NULL,        "get a configuration value" },
	{ CLI_OPT_TYPE_SWITCH,    "add",         0,  &action,       ACTION_ADD,
	  CLI_OPT_USAGE_CHOICE,    NULL,        "add a configuration value" },
	{ CLI_OPT_TYPE_SWITCH,    "replace-all", 0,  &action,       ACTION_REPLACE_ALL,
	  CLI_OPT_USAGE_CHOICE,    NULL,        "add a configuration value, replacing any old values" },
	{ CLI_OPT_TYPE_SWITCH,    "list",       'l', &action,       ACTION_LIST,
	  CLI_OPT_USAGE_CHOICE | CLI_OPT_USAGE_SHOW_LONG,
	                           NULL,        "list all configuration entries" },
	{ CLI_OPT_TYPE_SWITCH,    "show-origin", 0,  &show_origin,  1,
	  0,                       NULL,        "show origin of configuration" },
	{ CLI_OPT_TYPE_SWITCH,    "show-scope",  0,  &show_scope,   1,
	  0,                       NULL,        "show scope of configuration" },
	{ CLI_OPT_TYPE_ARG,       "name",        0,  &name,         0,
	  0,                      "name",       "name of configuration entry" },
	{ CLI_OPT_TYPE_ARG,       "value",       0,  &value,        0,
	  0,                      "value",      "value of configuration entry" },
	{ CLI_OPT_TYPE_ARG,       "regexp",      0,  &value_pattern, 0,
	  0,                      "regexp",     "regular expression of values to replace" },
	{ 0 },
};

static void print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, 0);
	printf("\n");

	printf("Query and set configuration options.\n");
	printf("\n");

	printf("Options:\n");

	cli_opt_help_fprint(stdout, opts);
}

static int get_config(git_config *config)
{
	git_buf value = GIT_BUF_INIT;
	char sep = null_separator ? '\0' : '\n';
	int error;

	error = git_config_get_string_buf(&value, config, name);

	if (error && error != GIT_ENOTFOUND)
		return cli_error_git();

	else if (error == GIT_ENOTFOUND)
		return 1;

	printf("%s%c", value.ptr, sep);
	return 0;
}

static int add_config(git_config *config)
{
	if (git_config_set_multivar(config, name, "$^", value) < 0)
		return cli_error_git();

	return 0;
}

static int replace_all_config(git_config *config)
{
	if (git_config_set_multivar(config, name, value_pattern ? value_pattern : ".*", value) < 0)
		return cli_error_git();

	return 0;
}

static const char *level_name(git_config_level_t level)
{
	switch (level) {
	case GIT_CONFIG_LEVEL_PROGRAMDATA:
		return "programdata";
	case GIT_CONFIG_LEVEL_SYSTEM:
		return "system";
	case GIT_CONFIG_LEVEL_XDG:
		return "global";
	case GIT_CONFIG_LEVEL_GLOBAL:
		return "global";
	case GIT_CONFIG_LEVEL_LOCAL:
		return "local";
	case GIT_CONFIG_LEVEL_APP:
		return "command";
	default:
		return "unknown";
	}
}

static int list_config(git_config *config)
{
	git_config_iterator *iterator;
	git_config_entry *entry;
	char data_separator = null_separator ? '\0' : '\t';
	char kv_separator = null_separator ? '\n' : '=';
	char entry_separator = null_separator ? '\0' : '\n';
	int error;

	if (git_config_iterator_new(&iterator, config) < 0)
		return cli_error_git();

	while ((error = git_config_next(&entry, iterator)) == 0) {
		if (show_scope)
			printf("%s%c",
				level_name(entry->level),
				data_separator);

		if (show_origin)
			printf("%s%s%s%c",
				entry->backend_type ? entry->backend_type : "",
				entry->backend_type && entry->origin_path ? ":" : "",
				entry->origin_path ? entry->origin_path : "",
				data_separator);

		printf("%s%c%s%c", entry->name, kv_separator, entry->value,
			entry_separator);
	}

	if (error != GIT_ITEROVER)
		return cli_error_git();

	git_config_iterator_free(iterator);
	return 0;
}

int cmd_config(int argc, char **argv)
{
	git_repository *repo = NULL;
	git_config *config = NULL;
	cli_repository_open_options open_opts = { argv + 1, argc - 1};
	cli_opt invalid_opt;
	int ret = 0;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	if (cli_opt__show_help) {
		print_help();
		return 0;
	}

	if (config_filename) {
		if (git_config_new(&config) < 0 ||
		    git_config_add_file_ondisk(config, config_filename,
				GIT_CONFIG_LEVEL_APP, NULL, 0) < 0) {
			ret = cli_error_git();
			goto done;
		}
	} else {
		if (cli_repository_open(&repo, &open_opts) < 0 ||
		    git_repository_config(&config, repo) < 0) {
			ret = cli_error_git();
			goto done;
		}

		if (config_level &&
		    git_config_open_level(&config, config, config_level) < 0) {
			ret = cli_error_git();
			goto done;
		}
	}

	switch (action) {
	case ACTION_ADD:
		if (!name || !value || value_pattern)
			ret = cli_error_usage("%s --add requires two arguments", COMMAND_NAME);
		else
			ret = add_config(config);
		break;
	case ACTION_REPLACE_ALL:
		if (!name || !value)
			ret = cli_error_usage("%s --replace-all requires two or three arguments", COMMAND_NAME);
		else
			ret = replace_all_config(config);
		break;
	case ACTION_GET:
		if (!name)
			ret = cli_error_usage("%s --get requires an argument", COMMAND_NAME);
		else
			ret = get_config(config);
		break;
	case ACTION_LIST:
		if (name)
			ret = cli_error_usage("%s --list does not take an argument", COMMAND_NAME);
		else
			ret = list_config(config);
		break;
	default:
		ret = cli_error_usage("unknown action");
	}

done:
	git_config_free(config);
	git_repository_free(repo);
	return ret;
}
