/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "attr_file.h"

#include "repository.h"
#include "filebuf.h"
#include "attrcache.h"
#include "git2/blob.h"
#include "git2/tree.h"
#include "blob.h"
#include "index.h"
#include "wildmatch.h"
#include <ctype.h>

static void attr_file_free(git_attr_file *file)
{
	bool unlock = !git_mutex_lock(&file->lock);
	git_attr_file__clear_rules(file, false);
	git_pool_clear(&file->pool);
	if (unlock)
		git_mutex_unlock(&file->lock);
	git_mutex_free(&file->lock);

	git__memzero(file, sizeof(*file));
	git__free(file);
}

int git_attr_file__new(
	git_attr_file **out,
	git_attr_file_entry *entry,
	git_attr_file_source *source)
{
	git_attr_file *attrs = git__calloc(1, sizeof(git_attr_file));
	GIT_ERROR_CHECK_ALLOC(attrs);

	if (git_mutex_init(&attrs->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to initialize lock");
		goto on_error;
	}

	if (git_pool_init(&attrs->pool, 1) < 0)
		goto on_error;

	GIT_REFCOUNT_INC(attrs);
	attrs->entry = entry;
	memcpy(&attrs->source, source, sizeof(git_attr_file_source));
	*out = attrs;
	return 0;

on_error:
	git__free(attrs);
	return -1;
}

int git_attr_file__clear_rules(git_attr_file *file, bool need_lock)
{
	unsigned int i;
	git_attr_rule *rule;

	if (need_lock && git_mutex_lock(&file->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock attribute file");
		return -1;
	}

	git_vector_foreach(&file->rules, i, rule)
		git_attr_rule__free(rule);
	git_vector_dispose(&file->rules);

	if (need_lock)
		git_mutex_unlock(&file->lock);

	return 0;
}

void git_attr_file__free(git_attr_file *file)
{
	if (!file)
		return;
	GIT_REFCOUNT_DEC(file, attr_file_free);
}

static int attr_file_oid_from_index(
	git_oid *oid, git_repository *repo, const char *path)
{
	int error;
	git_index *idx;
	size_t pos;
	const git_index_entry *entry;

	if ((error = git_repository_index__weakptr(&idx, repo)) < 0 ||
		(error = git_index__find_pos(&pos, idx, path, 0, 0)) < 0)
		return error;

	if (!(entry = git_index_get_byindex(idx, pos)))
		return GIT_ENOTFOUND;

	*oid = entry->id;
	return 0;
}

int git_attr_file__load(
	git_attr_file **out,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_entry *entry,
	git_attr_file_source *source,
	git_attr_file_parser parser,
	bool allow_macros)
{
	int error = 0;
	git_commit *commit = NULL;
	git_tree *tree = NULL;
	git_tree_entry *tree_entry = NULL;
	git_blob *blob = NULL;
	git_str content = GIT_STR_INIT;
	const char *content_str;
	git_attr_file *file;
	struct stat st;
	bool nonexistent = false;
	int bom_offset;
	git_str_bom_t bom;
	git_oid id;
	git_object_size_t blobsize;

	*out = NULL;

	switch (source->type) {
	case GIT_ATTR_FILE_SOURCE_MEMORY:
		/* in-memory attribute file doesn't need data */
		break;
	case GIT_ATTR_FILE_SOURCE_INDEX: {
		if ((error = attr_file_oid_from_index(&id, repo, entry->path)) < 0 ||
		    (error = git_blob_lookup(&blob, repo, &id)) < 0)
			return error;

		/* Do not assume that data straight from the ODB is NULL-terminated;
		 * copy the contents of a file to a buffer to work on */
		blobsize = git_blob_rawsize(blob);

		GIT_ERROR_CHECK_BLOBSIZE(blobsize);
		if (blobsize > GIT_ATTR_MAX_FILE_SIZE) /* TODO: issue warning when warning API is available */
			goto cleanup;
		git_str_put(&content, git_blob_rawcontent(blob), (size_t)blobsize);
		break;
	}
	case GIT_ATTR_FILE_SOURCE_FILE: {
		int fd = -1;

		/* For open or read errors, pretend that we got ENOTFOUND. */
		/* TODO: issue warning when warning API is available */

		if (p_stat(entry->fullpath, &st) < 0 ||
			S_ISDIR(st.st_mode) ||
			(fd = git_futils_open_ro(entry->fullpath)) < 0 ||
			(st.st_size > GIT_ATTR_MAX_FILE_SIZE) ||
			(error = git_futils_readbuffer_fd(&content, fd, (size_t)st.st_size)) < 0)
			nonexistent = true;

		if (fd >= 0)
			p_close(fd);

		break;
	}
	case GIT_ATTR_FILE_SOURCE_HEAD:
	case GIT_ATTR_FILE_SOURCE_COMMIT: {
		if (source->type == GIT_ATTR_FILE_SOURCE_COMMIT) {
			if ((error = git_commit_lookup(&commit, repo, source->commit_id)) < 0 ||
			    (error = git_commit_tree(&tree, commit)) < 0)
				goto cleanup;
		} else {
			if ((error = git_repository_head_tree(&tree, repo)) < 0)
				goto cleanup;
		}

		if ((error = git_tree_entry_bypath(&tree_entry, tree, entry->path)) < 0) {
			/*
			 * If the attributes file does not exist, we can
			 * cache an empty file for this commit to prevent
			 * needless future lookups.
			 */
			if (error == GIT_ENOTFOUND) {
				error = 0;
				break;
			}

			goto cleanup;
		}

		if ((error = git_blob_lookup(&blob, repo, git_tree_entry_id(tree_entry))) < 0)
			goto cleanup;

		/*
		 * Do not assume that data straight from the ODB is NULL-terminated;
		 * copy the contents of a file to a buffer to work on.
		 */
		blobsize = git_blob_rawsize(blob);

		GIT_ERROR_CHECK_BLOBSIZE(blobsize);
		if (blobsize > GIT_ATTR_MAX_FILE_SIZE) /* TODO: issue warning when warning API is available */
			goto cleanup;
		if ((error = git_str_put(&content,
			git_blob_rawcontent(blob), (size_t)blobsize)) < 0)
			goto cleanup;

		break;
	}
	default:
		git_error_set(GIT_ERROR_INVALID, "unknown file source %d", source->type);
		return -1;
	}

	if ((error = git_attr_file__new(&file, entry, source)) < 0)
		goto cleanup;

	/* advance over a UTF8 BOM */
	content_str = git_str_cstr(&content);
	bom_offset = git_str_detect_bom(&bom, &content);

	if (bom == GIT_STR_BOM_UTF8)
		content_str += bom_offset;

	/* store the key of the attr_reader; don't bother with cache
	 * invalidation during the same attr reader session.
	 */
	if (attr_session)
		file->session_key = attr_session->key;

	if (parser && (error = parser(repo, file, content_str, allow_macros)) < 0) {
		git_attr_file__free(file);
		goto cleanup;
	}

	/* write cache breakers */
	if (nonexistent)
		file->nonexistent = 1;
	else if (source->type == GIT_ATTR_FILE_SOURCE_INDEX)
		git_oid_cpy(&file->cache_data.oid, git_blob_id(blob));
	else if (source->type == GIT_ATTR_FILE_SOURCE_HEAD)
		git_oid_cpy(&file->cache_data.oid, git_tree_id(tree));
	else if (source->type == GIT_ATTR_FILE_SOURCE_COMMIT)
		git_oid_cpy(&file->cache_data.oid, git_tree_id(tree));
	else if (source->type == GIT_ATTR_FILE_SOURCE_FILE)
		git_futils_filestamp_set_from_stat(&file->cache_data.stamp, &st);
	/* else always cacheable */

	*out = file;

cleanup:
	git_blob_free(blob);
	git_tree_entry_free(tree_entry);
	git_tree_free(tree);
	git_commit_free(commit);
	git_str_dispose(&content);

	return error;
}

int git_attr_file__out_of_date(
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file *file,
	git_attr_file_source *source)
{
	if (!file)
		return 1;

	/* we are never out of date if we just created this data in the same
	 * attr_session; otherwise, nonexistent files must be invalidated
	 */
	if (attr_session && attr_session->key == file->session_key)
		return 0;
	else if (file->nonexistent)
		return 1;

	switch (file->source.type) {
	case GIT_ATTR_FILE_SOURCE_MEMORY:
		return 0;

	case GIT_ATTR_FILE_SOURCE_FILE:
		return git_futils_filestamp_check(
			&file->cache_data.stamp, file->entry->fullpath);

	case GIT_ATTR_FILE_SOURCE_INDEX: {
		int error;
		git_oid id;

		if ((error = attr_file_oid_from_index(
				&id, repo, file->entry->path)) < 0)
			return error;

		return (git_oid__cmp(&file->cache_data.oid, &id) != 0);
	}

	case GIT_ATTR_FILE_SOURCE_HEAD: {
		git_tree *tree = NULL;
		int error = git_repository_head_tree(&tree, repo);

		if (error < 0)
			return error;

		error = (git_oid__cmp(&file->cache_data.oid, git_tree_id(tree)) != 0);

		git_tree_free(tree);
		return error;
	}

	case GIT_ATTR_FILE_SOURCE_COMMIT: {
		git_commit *commit = NULL;
		git_tree *tree = NULL;
		int error;

		if ((error = git_commit_lookup(&commit, repo, source->commit_id)) < 0)
			return error;

		error = git_commit_tree(&tree, commit);
		git_commit_free(commit);

		if (error < 0)
			return error;

		error = (git_oid__cmp(&file->cache_data.oid, git_tree_id(tree)) != 0);

		git_tree_free(tree);
		return error;
	}

	default:
		git_error_set(GIT_ERROR_INVALID, "invalid file type %d", file->source.type);
		return -1;
	}
}

static int sort_by_hash_and_name(const void *a_raw, const void *b_raw);
static void git_attr_rule__clear(git_attr_rule *rule);
static bool parse_optimized_patterns(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *pattern);

int git_attr_file__parse_buffer(
	git_repository *repo, git_attr_file *attrs, const char *data, bool allow_macros)
{
	const char *scan = data, *context = NULL;
	git_attr_rule *rule = NULL;
	int error = 0;

	/* If subdir file path, convert context for file paths */
	if (attrs->entry && git_fs_path_root(attrs->entry->path) < 0 &&
	    !git__suffixcmp(attrs->entry->path, "/" GIT_ATTR_FILE))
		context = attrs->entry->path;

	if (git_mutex_lock(&attrs->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock attribute file");
		return -1;
	}

	while (!error && *scan) {
		/* Allocate rule if needed, otherwise re-use previous rule */
		if (!rule) {
			rule = git__calloc(1, sizeof(*rule));
			GIT_ERROR_CHECK_ALLOC(rule);
		} else
			git_attr_rule__clear(rule);

		rule->match.flags = GIT_ATTR_FNMATCH_ALLOWNEG | GIT_ATTR_FNMATCH_ALLOWMACRO;

		/* Parse the next "pattern attr attr attr" line */
		if ((error = git_attr_fnmatch__parse(&rule->match, &attrs->pool, context, &scan)) < 0 ||
		    (error = git_attr_assignment__parse(repo, &attrs->pool, &rule->assigns, &scan)) < 0)
		{
			if (error != GIT_ENOTFOUND)
				goto out;
			error = 0;
			continue;
		}

		if (rule->match.flags & GIT_ATTR_FNMATCH_MACRO) {
			/* TODO: warning if macro found in file below repo root */
			if (!allow_macros)
				continue;
			if ((error = git_attr_cache__insert_macro(repo, rule)) < 0)
				goto out;
		} else if ((error = git_vector_insert(&attrs->rules, rule)) < 0)
			goto out;

		rule = NULL;
	}

out:
	git_mutex_unlock(&attrs->lock);
	git_attr_rule__free(rule);

	return error;
}

uint32_t git_attr_file__name_hash(const char *name)
{
	uint32_t h = 5381;
	int c;

	GIT_ASSERT_ARG(name);

	while ((c = (int)*name++) != 0)
		h = ((h << 5) + h) + c;
	return h;
}

int git_attr_file__lookup_one(
	git_attr_file *file,
	git_attr_path *path,
	const char *attr,
	const char **value)
{
	size_t i;
	git_attr_name name;
	git_attr_rule *rule;

	*value = NULL;

	name.name = attr;
	name.name_hash = git_attr_file__name_hash(attr);

	git_attr_file__foreach_matching_rule(file, path, i, rule) {
		size_t pos;

		if (!git_vector_bsearch(&pos, &rule->assigns, &name)) {
			*value = ((git_attr_assignment *)
					  git_vector_get(&rule->assigns, pos))->value;
			break;
		}
	}

	return 0;
}

int git_attr_file__load_standalone(git_attr_file **out, const char *path)
{
	git_str content = GIT_STR_INIT;
	git_attr_file_source source = { GIT_ATTR_FILE_SOURCE_FILE };
	git_attr_file *file = NULL;
	int error;

	if ((error = git_futils_readbuffer(&content, path)) < 0)
		goto out;

	/*
	 * Because the cache entry is allocated from the file's own pool, we
	 * don't have to free it - freeing file+pool will free cache entry, too.
	 */

	if ((error = git_attr_file__new(&file, NULL, &source)) < 0 ||
	    (error = git_attr_file__parse_buffer(NULL, file, content.ptr, true)) < 0 ||
	    (error = git_attr_cache__alloc_file_entry(&file->entry, NULL, NULL, path, &file->pool)) < 0)
		goto out;

	*out = file;
out:
	if (error < 0)
		git_attr_file__free(file);
	git_str_dispose(&content);

	return error;
}

bool git_attr_fnmatch__match(
	git_attr_fnmatch *match,
	git_attr_path *path)
{
	const char *relpath = path->path;
	const char *filename;
	int flags = 0;

	/*
	 * If the rule was generated in a subdirectory, we must only
	 * use it for paths inside that directory. We can thus return
	 * a non-match if the prefixes don't match.
	 */
	if (match->containing_dir) {
		if (match->flags & GIT_ATTR_FNMATCH_ICASE) {
			if (git__strncasecmp(path->path, match->containing_dir, match->containing_dir_length))
				return 0;
		} else {
			if (git__prefixcmp(path->path, match->containing_dir))
				return 0;
		}

		relpath += match->containing_dir_length;
	}

	if (match->flags & GIT_ATTR_FNMATCH_ICASE)
		flags |= WM_CASEFOLD;

	if (match->flags & GIT_ATTR_FNMATCH_FULLPATH) {
		filename = relpath;
		flags |= WM_PATHNAME;
	} else {
		filename = path->basename;
	}

	if ((match->flags & GIT_ATTR_FNMATCH_DIRECTORY) && !path->is_dir) {
		bool samename;

		/*
		 * for attribute checks or checks at the root of this match's
		 * containing_dir (or root of the repository if no containing_dir),
		 * do not match.
		 */
		if (!(match->flags & GIT_ATTR_FNMATCH_IGNORE) ||
			path->basename == relpath)
			return false;

		/* fail match if this is a file with same name as ignored folder */
		samename = (match->flags & GIT_ATTR_FNMATCH_ICASE) ?
			!strcasecmp(match->pattern, relpath) :
			!strcmp(match->pattern, relpath);

		if (samename)
			return false;

		return (wildmatch(match->pattern, relpath, flags) == WM_MATCH);
	}

	return (wildmatch(match->pattern, filename, flags) == WM_MATCH);
}

bool git_attr_rule__match(
	git_attr_rule *rule,
	git_attr_path *path)
{
	bool matched = git_attr_fnmatch__match(&rule->match, path);

	if (rule->match.flags & GIT_ATTR_FNMATCH_NEGATIVE)
		matched = !matched;

	return matched;
}

git_attr_assignment *git_attr_rule__lookup_assignment(
	git_attr_rule *rule, const char *name)
{
	size_t pos;
	git_attr_name key;
	key.name = name;
	key.name_hash = git_attr_file__name_hash(name);

	if (git_vector_bsearch(&pos, &rule->assigns, &key))
		return NULL;

	return git_vector_get(&rule->assigns, pos);
}

int git_attr_path__init(
	git_attr_path *info,
	const char *path,
	const char *base,
	git_dir_flag dir_flag)
{
	ssize_t root;

	/* build full path as best we can */
	git_str_init(&info->full, 0);

	if (git_fs_path_join_unrooted(&info->full, path, base, &root) < 0)
		return -1;

	info->path = info->full.ptr + root;

	/* remove trailing slashes */
	while (info->full.size > 0) {
		if (info->full.ptr[info->full.size - 1] != '/')
			break;
		info->full.size--;
	}
	info->full.ptr[info->full.size] = '\0';

	/* skip leading slashes in path */
	while (*info->path == '/')
		info->path++;

	/* find trailing basename component */
	info->basename = strrchr(info->path, '/');
	if (info->basename)
		info->basename++;
	if (!info->basename || !*info->basename)
		info->basename = info->path;

	switch (dir_flag)
	{
	case GIT_DIR_FLAG_FALSE:
		info->is_dir = 0;
		break;

	case GIT_DIR_FLAG_TRUE:
		info->is_dir = 1;
		break;

	case GIT_DIR_FLAG_UNKNOWN:
	default:
		info->is_dir = (int)git_fs_path_isdir(info->full.ptr);
		break;
	}

	return 0;
}

void git_attr_path__free(git_attr_path *info)
{
	git_str_dispose(&info->full);
	info->path = NULL;
	info->basename = NULL;
}

/*
 * From gitattributes(5):
 *
 * Patterns have the following format:
 *
 * - A blank line matches no files, so it can serve as a separator for
 *   readability.
 *
 * - A line starting with # serves as a comment.
 *
 * - An optional prefix ! which negates the pattern; any matching file
 *   excluded by a previous pattern will become included again. If a negated
 *   pattern matches, this will override lower precedence patterns sources.
 *
 * - If the pattern ends with a slash, it is removed for the purpose of the
 *   following description, but it would only find a match with a directory. In
 *   other words, foo/ will match a directory foo and paths underneath it, but
 *   will not match a regular file or a symbolic link foo (this is consistent
 *   with the way how pathspec works in general in git).
 *
 * - If the pattern does not contain a slash /, git treats it as a shell glob
 *   pattern and checks for a match against the pathname without leading
 *   directories.
 *
 * - Otherwise, git treats the pattern as a shell glob suitable for consumption
 *   by fnmatch(3) with the FNM_PATHNAME flag: wildcards in the pattern will
 *   not match a / in the pathname. For example, "Documentation/\*.html" matches
 *   "Documentation/git.html" but not "Documentation/ppc/ppc.html". A leading
 *   slash matches the beginning of the pathname; for example, "/\*.c" matches
 *   "cat-file.c" but not "mozilla-sha1/sha1.c".
 */

/*
 * Determine the length of trailing spaces. Escaped spaces do not count as
 * trailing whitespace.
 */
static size_t trailing_space_length(const char *p, size_t len)
{
	size_t n, i;
	for (n = len; n; n--) {
		if (p[n-1] != ' ' && p[n-1] != '\t')
			break;

		/*
		 * Count escape-characters before space. In case where it's an
		 * even number of escape characters, then the escape char itself
		 * is escaped and the whitespace is an unescaped whitespace.
		 * Otherwise, the last escape char is not escaped and the
		 * whitespace in an escaped whitespace.
		 */
		i = n;
		while (i > 1 && p[i-2] == '\\')
			i--;
		if ((n - i) % 2)
			break;
	}
	return len - n;
}

static size_t unescape_spaces(char *str)
{
	char *scan, *pos = str;
	bool escaped = false;

	if (!str)
		return 0;

	for (scan = str; *scan; scan++) {
		if (!escaped && *scan == '\\') {
			escaped = true;
			continue;
		}

		/* Only insert the escape character for escaped non-spaces */
		if (escaped && !git__isspace(*scan))
			*pos++ = '\\';

		*pos++ = *scan;
		escaped = false;
	}

	if (pos != scan)
		*pos = '\0';

	return (pos - str);
}

/*
 * This will return 0 if the spec was filled out,
 * GIT_ENOTFOUND if the fnmatch does not require matching, or
 * another error code there was an actual problem.
 */
int git_attr_fnmatch__parse(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *context,
	const char **base)
{
	const char *pattern, *scan;
	int slash_count, allow_space;
	bool escaped;

	GIT_ASSERT_ARG(spec);
	GIT_ASSERT_ARG(base && *base);

	if (parse_optimized_patterns(spec, pool, *base))
		return 0;

	spec->flags = (spec->flags & GIT_ATTR_FNMATCH__INCOMING);
	allow_space = ((spec->flags & GIT_ATTR_FNMATCH_ALLOWSPACE) != 0);

	pattern = *base;

	while (!allow_space && git__isspace(*pattern))
		pattern++;

	if (!*pattern || *pattern == '#' || *pattern == '\n' ||
	    (*pattern == '\r' && *(pattern + 1) == '\n')) {
		*base = git__next_line(pattern);
		return GIT_ENOTFOUND;
	}

	if (*pattern == '[' && (spec->flags & GIT_ATTR_FNMATCH_ALLOWMACRO) != 0) {
		if (strncmp(pattern, "[attr]", 6) == 0) {
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_MACRO;
			pattern += 6;
		}
		/* else a character range like [a-e]* which is accepted */
	}

	if (*pattern == '!' && (spec->flags & GIT_ATTR_FNMATCH_ALLOWNEG) != 0) {
		spec->flags = spec->flags | GIT_ATTR_FNMATCH_NEGATIVE;
		pattern++;
	}

	slash_count = 0;
	escaped = false;
	/* Scan until a non-escaped whitespace. */
	for (scan = pattern; *scan != '\0'; ++scan) {
		char c = *scan;

		if (c == '\\' && !escaped) {
			escaped = true;
			continue;
		} else if (git__isspace(c) && !escaped) {
			if (!allow_space || (c != ' ' && c != '\t' && c != '\r'))
				break;
		} else if (c == '/') {
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_FULLPATH;
			slash_count++;

			if (slash_count == 1 && pattern == scan)
				pattern++;
		} else if (git__iswildcard(c) && !escaped) {
			/* remember if we see an unescaped wildcard in pattern */
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_HASWILD;
		}

		escaped = false;
	}

	*base = scan;

	if ((spec->length = scan - pattern) == 0)
		return GIT_ENOTFOUND;

	/*
	 * Remove one trailing \r in case this is a CRLF delimited
	 * file, in the case of Icon\r\r\n, we still leave the first
	 * \r there to match against.
	 */
	if (pattern[spec->length - 1] == '\r')
		if (--spec->length == 0)
			return GIT_ENOTFOUND;

	/* Remove trailing spaces. */
	spec->length -= trailing_space_length(pattern, spec->length);

	if (spec->length == 0)
		return GIT_ENOTFOUND;

	if (pattern[spec->length - 1] == '/') {
		spec->length--;
		spec->flags = spec->flags | GIT_ATTR_FNMATCH_DIRECTORY;
		if (--slash_count <= 0)
			spec->flags = spec->flags & ~GIT_ATTR_FNMATCH_FULLPATH;
	}

	if (context) {
		char *slash = strrchr(context, '/');
		size_t len;
		if (slash) {
			/* include the slash for easier matching */
			len = slash - context + 1;
			spec->containing_dir = git_pool_strndup(pool, context, len);
			spec->containing_dir_length = len;
		}
	}

	spec->pattern = git_pool_strndup(pool, pattern, spec->length);

	if (!spec->pattern) {
		*base = git__next_line(pattern);
		return -1;
	} else {
		/* strip '\' that might have been used for internal whitespace */
		spec->length = unescape_spaces(spec->pattern);
	}

	return 0;
}

static bool parse_optimized_patterns(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *pattern)
{
	if (!pattern[1] && (pattern[0] == '*' || pattern[0] == '.')) {
		spec->flags = GIT_ATTR_FNMATCH_MATCH_ALL;
		spec->pattern = git_pool_strndup(pool, pattern, 1);
		spec->length = 1;

		return true;
	}

	return false;
}

static int sort_by_hash_and_name(const void *a_raw, const void *b_raw)
{
	const git_attr_name *a = a_raw;
	const git_attr_name *b = b_raw;

	if (b->name_hash < a->name_hash)
		return 1;
	else if (b->name_hash > a->name_hash)
		return -1;
	else
		return strcmp(b->name, a->name);
}

static void git_attr_assignment__free(git_attr_assignment *assign)
{
	/* name and value are stored in a git_pool associated with the
	 * git_attr_file, so they do not need to be freed here
	 */
	assign->name = NULL;
	assign->value = NULL;
	git__free(assign);
}

static int merge_assignments(void **old_raw, void *new_raw)
{
	git_attr_assignment **old = (git_attr_assignment **)old_raw;
	git_attr_assignment *new = (git_attr_assignment *)new_raw;

	GIT_REFCOUNT_DEC(*old, git_attr_assignment__free);
	*old = new;
	return GIT_EEXISTS;
}

int git_attr_assignment__parse(
	git_repository *repo,
	git_pool *pool,
	git_vector *assigns,
	const char **base)
{
	int error;
	const char *scan = *base;
	git_attr_assignment *assign = NULL;

	GIT_ASSERT_ARG(assigns && !assigns->length);

	git_vector_set_cmp(assigns, sort_by_hash_and_name);

	while (*scan && *scan != '\n') {
		const char *name_start, *value_start;

		/* skip leading blanks */
		while (git__isspace(*scan) && *scan != '\n') scan++;

		/* allocate assign if needed */
		if (!assign) {
			assign = git__calloc(1, sizeof(git_attr_assignment));
			GIT_ERROR_CHECK_ALLOC(assign);
			GIT_REFCOUNT_INC(assign);
		}

		assign->name_hash = 5381;
		assign->value = git_attr__true;

		/* look for magic name prefixes */
		if (*scan == '-') {
			assign->value = git_attr__false;
			scan++;
		} else if (*scan == '!') {
			assign->value = git_attr__unset; /* explicit unspecified state */
			scan++;
		} else if (*scan == '#') /* comment rest of line */
			break;

		/* find the name */
		name_start = scan;
		while (*scan && !git__isspace(*scan) && *scan != '=') {
			assign->name_hash =
				((assign->name_hash << 5) + assign->name_hash) + *scan;
			scan++;
		}
		if (scan == name_start) {
			/* must have found lone prefix (" - ") or leading = ("=foo")
			 * or end of buffer -- advance until whitespace and continue
			 */
			while (*scan && !git__isspace(*scan)) scan++;
			continue;
		}

		/* allocate permanent storage for name */
		assign->name = git_pool_strndup(pool, name_start, scan - name_start);
		GIT_ERROR_CHECK_ALLOC(assign->name);

		/* if there is an equals sign, find the value */
		if (*scan == '=') {
			for (value_start = ++scan; *scan && !git__isspace(*scan); ++scan);

			/* if we found a value, allocate permanent storage for it */
			if (scan > value_start) {
				assign->value = git_pool_strndup(pool, value_start, scan - value_start);
				GIT_ERROR_CHECK_ALLOC(assign->value);
			}
		}

		/* expand macros (if given a repo with a macro cache) */
		if (repo != NULL && assign->value == git_attr__true) {
			git_attr_rule *macro =
				git_attr_cache__lookup_macro(repo, assign->name);

			if (macro != NULL) {
				unsigned int i;
				git_attr_assignment *massign;

				git_vector_foreach(&macro->assigns, i, massign) {
					GIT_REFCOUNT_INC(massign);

					error = git_vector_insert_sorted(
						assigns, massign, &merge_assignments);
					if (error < 0 && error != GIT_EEXISTS) {
						git_attr_assignment__free(assign);
						return error;
					}
				}
			}
		}

		/* insert allocated assign into vector */
		error = git_vector_insert_sorted(assigns, assign, &merge_assignments);
		if (error < 0 && error != GIT_EEXISTS)
			return error;

		/* clear assign since it is now "owned" by the vector */
		assign = NULL;
	}

	if (assign != NULL)
		git_attr_assignment__free(assign);

	*base = git__next_line(scan);

	return (assigns->length == 0) ? GIT_ENOTFOUND : 0;
}

static void git_attr_rule__clear(git_attr_rule *rule)
{
	unsigned int i;
	git_attr_assignment *assign;

	if (!rule)
		return;

	if (!(rule->match.flags & GIT_ATTR_FNMATCH_IGNORE)) {
		git_vector_foreach(&rule->assigns, i, assign)
			GIT_REFCOUNT_DEC(assign, git_attr_assignment__free);
		git_vector_dispose(&rule->assigns);
	}

	/* match.pattern is stored in a git_pool, so no need to free */
	rule->match.pattern = NULL;
	rule->match.length = 0;
}

void git_attr_rule__free(git_attr_rule *rule)
{
	git_attr_rule__clear(rule);
	git__free(rule);
}

int git_attr_session__init(git_attr_session *session, git_repository *repo)
{
	GIT_ASSERT_ARG(repo);

	memset(session, 0, sizeof(*session));
	session->key = git_atomic32_inc(&repo->attr_session_key);

	return 0;
}

void git_attr_session__free(git_attr_session *session)
{
	if (!session)
		return;

	git_str_dispose(&session->sysdir);
	git_str_dispose(&session->tmp);

	memset(session, 0, sizeof(git_attr_session));
}
