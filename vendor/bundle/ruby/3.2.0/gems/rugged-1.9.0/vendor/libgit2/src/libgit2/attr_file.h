/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_attr_file_h__
#define INCLUDE_attr_file_h__

#include "common.h"

#include "git2/oid.h"
#include "git2/attr.h"
#include "vector.h"
#include "pool.h"
#include "str.h"
#include "futils.h"

#define GIT_ATTR_FILE			".gitattributes"
#define GIT_ATTR_FILE_INREPO	"attributes"
#define GIT_ATTR_FILE_SYSTEM	"gitattributes"
#define GIT_ATTR_FILE_XDG		"attributes"

#define GIT_ATTR_MAX_FILE_SIZE	100 * 1024 * 1024

#define GIT_ATTR_FNMATCH_NEGATIVE	(1U << 0)
#define GIT_ATTR_FNMATCH_DIRECTORY	(1U << 1)
#define GIT_ATTR_FNMATCH_FULLPATH	(1U << 2)
#define GIT_ATTR_FNMATCH_MACRO		(1U << 3)
#define GIT_ATTR_FNMATCH_IGNORE		(1U << 4)
#define GIT_ATTR_FNMATCH_HASWILD	(1U << 5)
#define GIT_ATTR_FNMATCH_ALLOWSPACE	(1U << 6)
#define GIT_ATTR_FNMATCH_ICASE		(1U << 7)
#define GIT_ATTR_FNMATCH_MATCH_ALL	(1U << 8)
#define GIT_ATTR_FNMATCH_ALLOWNEG   (1U << 9)
#define GIT_ATTR_FNMATCH_ALLOWMACRO (1U << 10)

#define GIT_ATTR_FNMATCH__INCOMING \
	(GIT_ATTR_FNMATCH_ALLOWSPACE | GIT_ATTR_FNMATCH_ALLOWNEG | GIT_ATTR_FNMATCH_ALLOWMACRO)

typedef enum {
	GIT_ATTR_FILE_SOURCE_MEMORY = 0,
	GIT_ATTR_FILE_SOURCE_FILE   = 1,
	GIT_ATTR_FILE_SOURCE_INDEX  = 2,
	GIT_ATTR_FILE_SOURCE_HEAD   = 3,
	GIT_ATTR_FILE_SOURCE_COMMIT = 4,

	GIT_ATTR_FILE_NUM_SOURCES   = 5
} git_attr_file_source_t;

typedef struct {
	/* The source location for the attribute file. */
	git_attr_file_source_t type;

	/*
	 * The filename of the attribute file to read (relative to the
	 * given base path).
	 */
	const char *base;
	const char *filename;

	/*
	 * The commit ID when the given source type is a commit (or NULL
	 * for the repository's HEAD commit.)
	 */
	git_oid *commit_id;
} git_attr_file_source;

extern const char *git_attr__true;
extern const char *git_attr__false;
extern const char *git_attr__unset;

typedef struct {
	char *pattern;
	size_t length;
	char *containing_dir;
	size_t containing_dir_length;
	unsigned int flags;
} git_attr_fnmatch;

typedef struct {
	git_attr_fnmatch match;
	git_vector assigns;		/* vector of <git_attr_assignment*> */
} git_attr_rule;

typedef struct {
	git_refcount unused;
	const char *name;
	uint32_t name_hash;
} git_attr_name;

typedef struct {
	git_refcount rc;		/* for macros */
	char *name;
	uint32_t name_hash;
	const char *value;
} git_attr_assignment;

typedef struct git_attr_file_entry git_attr_file_entry;

typedef struct {
	git_refcount rc;
	git_mutex lock;
	git_attr_file_entry *entry;
	git_attr_file_source source;
	git_vector rules;			/* vector of <rule*> or <fnmatch*> */
	git_pool pool;
	unsigned int nonexistent:1;
	int session_key;
	union {
		git_oid oid;
		git_futils_filestamp stamp;
	} cache_data;
} git_attr_file;

struct git_attr_file_entry {
	git_attr_file *file[GIT_ATTR_FILE_NUM_SOURCES];
	const char *path; /* points into fullpath */
	char fullpath[GIT_FLEX_ARRAY];
};

typedef struct {
	git_str  full;
	char    *path;
	char    *basename;
	int      is_dir;
} git_attr_path;

/* A git_attr_session can provide an "instance" of reading, to prevent cache
 * invalidation during a single operation instance (like checkout).
 */

typedef struct {
	int key;
	unsigned int init_setup:1,
		init_sysdir:1;
	git_str sysdir;
	git_str tmp;
} git_attr_session;

extern int git_attr_session__init(git_attr_session *attr_session, git_repository *repo);
extern void git_attr_session__free(git_attr_session *session);

extern int git_attr_get_many_with_session(
	const char **values_out,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_options *opts,
	const char *path,
	size_t num_attr,
	const char **names);

typedef int (*git_attr_file_parser)(
	git_repository *repo,
	git_attr_file *file,
	const char *data,
	bool allow_macros);

/*
 * git_attr_file API
 */

int git_attr_file__new(
	git_attr_file **out,
	git_attr_file_entry *entry,
	git_attr_file_source *source);

void git_attr_file__free(git_attr_file *file);

int git_attr_file__load(
	git_attr_file **out,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_entry *ce,
	git_attr_file_source *source,
	git_attr_file_parser parser,
	bool allow_macros);

int git_attr_file__load_standalone(
	git_attr_file **out, const char *path);

int git_attr_file__out_of_date(
	git_repository *repo, git_attr_session *session, git_attr_file *file, git_attr_file_source *source);

int git_attr_file__parse_buffer(
	git_repository *repo, git_attr_file *attrs, const char *data, bool allow_macros);

int git_attr_file__clear_rules(
	git_attr_file *file, bool need_lock);

int git_attr_file__lookup_one(
	git_attr_file *file,
	git_attr_path *path,
	const char *attr,
	const char **value);

/* loop over rules in file from bottom to top */
#define git_attr_file__foreach_matching_rule(file, path, iter, rule)	\
	git_vector_rforeach(&(file)->rules, (iter), (rule)) \
		if (git_attr_rule__match((rule), (path)))

uint32_t git_attr_file__name_hash(const char *name);


/*
 * other utilities
 */

extern int git_attr_fnmatch__parse(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *source,
	const char **base);

extern bool git_attr_fnmatch__match(
	git_attr_fnmatch *rule,
	git_attr_path *path);

extern void git_attr_rule__free(git_attr_rule *rule);

extern bool git_attr_rule__match(
	git_attr_rule *rule,
	git_attr_path *path);

extern git_attr_assignment *git_attr_rule__lookup_assignment(
	git_attr_rule *rule, const char *name);

typedef enum { GIT_DIR_FLAG_TRUE = 1, GIT_DIR_FLAG_FALSE = 0, GIT_DIR_FLAG_UNKNOWN = -1 } git_dir_flag;

extern int git_attr_path__init(
	git_attr_path *out,
	const char *path,
	const char *base,
	git_dir_flag is_dir);
extern void git_attr_path__free(git_attr_path *info);

extern int git_attr_assignment__parse(
	git_repository *repo, /* needed to expand macros */
	git_pool *pool,
	git_vector *assigns,
	const char **scan);

#endif
