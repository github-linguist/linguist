/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_repository_h__
#define INCLUDE_repository_h__

#include "common.h"

#include "git2/common.h"
#include "git2/oid.h"
#include "git2/odb.h"
#include "git2/repository.h"
#include "git2/object.h"
#include "git2/config.h"

#include "array.h"
#include "cache.h"
#include "refs.h"
#include "str.h"
#include "object.h"
#include "attrcache.h"
#include "submodule.h"
#include "diff_driver.h"
#include "grafts.h"

#define DOT_GIT ".git"
#define GIT_DIR DOT_GIT "/"
#define GIT_DIR_MODE 0755
#define GIT_BARE_DIR_MODE 0777

/* Default DOS-compatible 8.3 "short name" for a git repository, "GIT~1" */
#define GIT_DIR_SHORTNAME "GIT~1"

extern bool git_repository__fsync_gitdir;
extern bool git_repository__validate_ownership;

/** Cvar cache identifiers */
typedef enum {
	GIT_CONFIGMAP_AUTO_CRLF = 0,    /* core.autocrlf */
	GIT_CONFIGMAP_EOL,              /* core.eol */
	GIT_CONFIGMAP_SYMLINKS,         /* core.symlinks */
	GIT_CONFIGMAP_IGNORECASE,       /* core.ignorecase */
	GIT_CONFIGMAP_FILEMODE,         /* core.filemode */
	GIT_CONFIGMAP_IGNORESTAT,       /* core.ignorestat */
	GIT_CONFIGMAP_TRUSTCTIME,       /* core.trustctime */
	GIT_CONFIGMAP_ABBREV,           /* core.abbrev */
	GIT_CONFIGMAP_PRECOMPOSE,       /* core.precomposeunicode */
	GIT_CONFIGMAP_SAFE_CRLF,		/* core.safecrlf */
	GIT_CONFIGMAP_LOGALLREFUPDATES, /* core.logallrefupdates */
	GIT_CONFIGMAP_PROTECTHFS,       /* core.protectHFS */
	GIT_CONFIGMAP_PROTECTNTFS,      /* core.protectNTFS */
	GIT_CONFIGMAP_FSYNCOBJECTFILES, /* core.fsyncObjectFiles */
	GIT_CONFIGMAP_LONGPATHS,        /* core.longpaths */
	GIT_CONFIGMAP_CACHE_MAX
} git_configmap_item;

/**
 * Configuration map value enumerations
 *
 * These are the values that are actually stored in the configmap cache,
 * instead of their string equivalents. These values are internal and
 * symbolic; make sure that none of them is set to `-1`, since that is
 * the unique identifier for "not cached"
 */
typedef enum {
	/* The value hasn't been loaded from the cache yet */
	GIT_CONFIGMAP_NOT_CACHED = -1,

	/* core.safecrlf: false, 'fail', 'warn' */
	GIT_SAFE_CRLF_FALSE = 0,
	GIT_SAFE_CRLF_FAIL = 1,
	GIT_SAFE_CRLF_WARN = 2,

	/* core.autocrlf: false, true, 'input; */
	GIT_AUTO_CRLF_FALSE = 0,
	GIT_AUTO_CRLF_TRUE = 1,
	GIT_AUTO_CRLF_INPUT = 2,
	GIT_AUTO_CRLF_DEFAULT = GIT_AUTO_CRLF_FALSE,

	/* core.eol: unset, 'crlf', 'lf', 'native' */
	GIT_EOL_UNSET = 0,
	GIT_EOL_CRLF = 1,
	GIT_EOL_LF = 2,
#ifdef GIT_WIN32
	GIT_EOL_NATIVE = GIT_EOL_CRLF,
#else
	GIT_EOL_NATIVE = GIT_EOL_LF,
#endif
	GIT_EOL_DEFAULT = GIT_EOL_NATIVE,

	/* core.symlinks: bool */
	GIT_SYMLINKS_DEFAULT = GIT_CONFIGMAP_TRUE,
	/* core.ignorecase */
	GIT_IGNORECASE_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.filemode */
	GIT_FILEMODE_DEFAULT = GIT_CONFIGMAP_TRUE,
	/* core.ignorestat */
	GIT_IGNORESTAT_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.trustctime */
	GIT_TRUSTCTIME_DEFAULT = GIT_CONFIGMAP_TRUE,
	/* core.abbrev */
	GIT_ABBREV_FALSE = GIT_OID_MAX_HEXSIZE,
	GIT_ABBREV_MINIMUM = 4,
	GIT_ABBREV_DEFAULT = 7,
	/* core.precomposeunicode */
	GIT_PRECOMPOSE_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.safecrlf */
	GIT_SAFE_CRLF_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.logallrefupdates */
	GIT_LOGALLREFUPDATES_FALSE = GIT_CONFIGMAP_FALSE,
	GIT_LOGALLREFUPDATES_TRUE = GIT_CONFIGMAP_TRUE,
	GIT_LOGALLREFUPDATES_UNSET = 2,
	GIT_LOGALLREFUPDATES_ALWAYS = 3,
	GIT_LOGALLREFUPDATES_DEFAULT = GIT_LOGALLREFUPDATES_UNSET,
	/* core.protectHFS */
	GIT_PROTECTHFS_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.protectNTFS */
	GIT_PROTECTNTFS_DEFAULT = GIT_CONFIGMAP_TRUE,
	/* core.fsyncObjectFiles */
	GIT_FSYNCOBJECTFILES_DEFAULT = GIT_CONFIGMAP_FALSE,
	/* core.longpaths */
	GIT_LONGPATHS_DEFAULT = GIT_CONFIGMAP_FALSE
} git_configmap_value;

/* internal repository init flags */
enum {
	GIT_REPOSITORY_INIT__HAS_DOTGIT = (1u << 16),
	GIT_REPOSITORY_INIT__NATURAL_WD = (1u << 17),
	GIT_REPOSITORY_INIT__IS_REINIT  = (1u << 18)
};

/** Internal structure for repository object */
struct git_repository {
	git_odb *_odb;
	git_refdb *_refdb;
	git_config *_config;
	git_index *_index;

	git_cache objects;
	git_attr_cache *attrcache;
	git_diff_driver_registry *diff_drivers;

	char *gitlink;
	char *gitdir;
	char *commondir;
	char *workdir;
	char *namespace;

	char *ident_name;
	char *ident_email;

	git_array_t(git_str) reserved_names;

	unsigned use_env:1,
	         is_bare:1,
	         is_worktree:1;
	git_oid_t oid_type;

	unsigned int lru_counter;

	git_grafts *grafts;
	git_grafts *shallow_grafts;

	git_atomic32 attr_session_key;

	intptr_t configmap_cache[GIT_CONFIGMAP_CACHE_MAX];
	git_submodule_cache *submodule_cache;
};

GIT_INLINE(git_attr_cache *) git_repository_attr_cache(git_repository *repo)
{
	return repo->attrcache;
}

int git_repository_head_commit(git_commit **commit, git_repository *repo);
int git_repository_head_tree(git_tree **tree, git_repository *repo);
int git_repository_create_head(const char *git_dir, const char *ref_name);

typedef int (*git_repository_foreach_worktree_cb)(git_repository *, void *);

int git_repository_foreach_worktree(git_repository *repo,
				    git_repository_foreach_worktree_cb cb,
				    void *payload);

/*
 * Weak pointers to repository internals.
 *
 * The returned pointers do not need to be freed. Do not keep
 * permanent references to these (i.e. between API calls), since they may
 * become invalidated if the user replaces a repository internal.
 */
int git_repository_config__weakptr(git_config **out, git_repository *repo);
int git_repository_odb__weakptr(git_odb **out, git_repository *repo);
int git_repository_refdb__weakptr(git_refdb **out, git_repository *repo);
int git_repository_index__weakptr(git_index **out, git_repository *repo);
int git_repository_grafts__weakptr(git_grafts **out, git_repository *repo);
int git_repository_shallow_grafts__weakptr(git_grafts **out, git_repository *repo);

/*
 * Configuration map cache
 *
 * Efficient access to the most used config variables of a repository.
 * The cache is cleared every time the config backend is replaced.
 */
int git_repository__configmap_lookup(int *out, git_repository *repo, git_configmap_item item);
void git_repository__configmap_lookup_cache_clear(git_repository *repo);

/** Return the length that object names will be abbreviated to. */
int git_repository__abbrev_length(int *out, git_repository *repo);

int git_repository__item_path(git_str *out, const git_repository *repo, git_repository_item_t item);

GIT_INLINE(int) git_repository__ensure_not_bare(
	git_repository *repo,
	const char *operation_name)
{
	if (!git_repository_is_bare(repo))
		return 0;

	git_error_set(
		GIT_ERROR_REPOSITORY,
		"cannot %s. This operation is not allowed against bare repositories.",
		operation_name);

	return GIT_EBAREREPO;
}

int git_repository__set_orig_head(git_repository *repo, const git_oid *orig_head);

int git_repository__cleanup_files(git_repository *repo, const char *files[], size_t files_len);

/* The default "reserved names" for a repository */
extern git_str git_repository__reserved_names_win32[];
extern size_t git_repository__reserved_names_win32_len;

extern git_str git_repository__reserved_names_posix[];
extern size_t git_repository__reserved_names_posix_len;

/*
 * Gets any "reserved names" in the repository.  This will return paths
 * that should not be allowed in the repository (like ".git") to avoid
 * conflicting with the repository path, or with alternate mechanisms to
 * the repository path (eg, "GIT~1").  Every attempt will be made to look
 * up all possible reserved names - if there was a conflict for the shortname
 * GIT~1, for example, this function will try to look up the alternate
 * shortname.  If that fails, this function returns false, but out and outlen
 * will still be populated with good defaults.
 */
bool git_repository__reserved_names(
	git_str **out, size_t *outlen, git_repository *repo, bool include_ntfs);

int git_repository__shallow_roots(git_oid **out, size_t *out_len, git_repository *repo);
int git_repository__shallow_roots_write(git_repository *repo, git_oidarray *roots);

/*
 * The default branch for the repository; the `init.defaultBranch`
 * configuration option, if set, or `master` if it is not.
 */
int git_repository_initialbranch(git_str *out, git_repository *repo);

/*
 * Given a relative `path`, this makes it absolute based on the
 * repository's working directory.  This will perform validation
 * to ensure that the path is not longer than MAX_PATH on Windows
 * (unless `core.longpaths` is set in the repo config).
 */
int git_repository_workdir_path(git_str *out, git_repository *repo, const char *path);

int git_repository__extensions(char ***out, size_t *out_len);
int git_repository__set_extensions(const char **extensions, size_t len);
void git_repository__free_extensions(void);

/*
 * Set the object format (OID type) for a repository; this will set
 * both the configuration and the internal value for the oid type.
 */
int git_repository__set_objectformat(
	git_repository *repo,
	git_oid_t oid_type);

/* SHA256-aware internal functions */
int git_repository__new(git_repository **out, git_oid_t oid_type);

#endif
