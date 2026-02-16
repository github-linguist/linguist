/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "refs.h"
#include "hash.h"
#include "repository.h"
#include "futils.h"
#include "filebuf.h"
#include "pack.h"
#include "parse.h"
#include "reflog.h"
#include "refdb.h"
#include "iterator.h"
#include "sortedcache.h"
#include "signature.h"
#include "wildmatch.h"
#include "path.h"

#include <git2/tag.h>
#include <git2/object.h>
#include <git2/refdb.h>
#include <git2/branch.h>
#include <git2/sys/refdb_backend.h>
#include <git2/sys/refs.h>

#define DEFAULT_NESTING_LEVEL	5
#define MAX_NESTING_LEVEL		10

enum {
	PACKREF_HAS_PEEL = 1,
	PACKREF_WAS_LOOSE = 2,
	PACKREF_CANNOT_PEEL = 4,
	PACKREF_SHADOWED = 8
};

enum {
	PEELING_NONE = 0,
	PEELING_STANDARD,
	PEELING_FULL
};

struct packref {
	git_oid oid;
	git_oid peel;
	char flags;
	char name[GIT_FLEX_ARRAY];
};

typedef struct refdb_fs_backend {
	git_refdb_backend parent;

	git_repository *repo;
	/* path to git directory */
	char *gitpath;
	/* path to common objects' directory */
	char *commonpath;

	git_oid_t oid_type;

	unsigned int fsync : 1,
	             sorted : 1;
	int peeling_mode;
	git_iterator_flag_t iterator_flags;
	uint32_t direach_flags;
	git_sortedcache *refcache;
	git_map packed_refs_map;
	git_mutex prlock; /* protect packed_refs_map */
	git_futils_filestamp packed_refs_stamp;
} refdb_fs_backend;

static int refdb_reflog_fs__delete(git_refdb_backend *_backend, const char *name);
static char *packed_set_peeling_mode(char *data, size_t data_sz, refdb_fs_backend *backend);

GIT_INLINE(int) loose_path(
	git_str *out,
	const char *base,
	const char *refname)
{
	if (git_str_joinpath(out, base, refname) < 0)
		return -1;

	return git_fs_path_validate_str_length_with_suffix(out,
		CONST_STRLEN(".lock"));
}

GIT_INLINE(int) reflog_path(
	git_str *out,
	git_repository *repo,
	const char *refname)
{
	const char *base;
	int error;

	base = (strcmp(refname, GIT_HEAD_FILE) == 0) ? repo->gitdir :
		repo->commondir;

	if ((error = git_str_joinpath(out, base, GIT_REFLOG_DIR)) < 0)
		return error;

	return loose_path(out, out->ptr, refname);
}

static int packref_cmp(const void *a_, const void *b_)
{
	const struct packref *a = a_, *b = b_;
	return strcmp(a->name, b->name);
}

static int packed_reload(refdb_fs_backend *backend)
{
	int error;
	git_str packedrefs = GIT_STR_INIT;
	size_t oid_hexsize = git_oid_hexsize(backend->oid_type);
	char *scan, *eof, *eol;

	if (!backend->gitpath)
		return 0;

	error = git_sortedcache_lockandload(backend->refcache, &packedrefs);

	/*
	 * If we can't find the packed-refs, clear table and return.
	 * Any other error just gets passed through.
	 * If no error, and file wasn't changed, just return.
	 * Anything else means we need to refresh the packed refs.
	 */
	if (error <= 0) {
		if (error == GIT_ENOTFOUND) {
			GIT_UNUSED(git_sortedcache_clear(backend->refcache, true));
			git_error_clear();
			error = 0;
		}
		return error;
	}

	/* At this point, refresh the packed refs from the loaded buffer. */

	GIT_UNUSED(git_sortedcache_clear(backend->refcache, false));

	scan = packedrefs.ptr;
	eof  = scan + packedrefs.size;

	scan = packed_set_peeling_mode(scan, packedrefs.size, backend);
	if (!scan)
		goto parse_failed;

	while (scan < eof && *scan == '#') {
		if (!(eol = strchr(scan, '\n')))
			goto parse_failed;
		scan = eol + 1;
	}

	while (scan < eof) {
		struct packref *ref;
		git_oid oid;

		/* parse "<OID> <refname>\n" */

		if (git_oid__fromstr(&oid, scan, backend->oid_type) < 0)
			goto parse_failed;
		scan += oid_hexsize;

		if (*scan++ != ' ')
			goto parse_failed;
		if (!(eol = strchr(scan, '\n')))
			goto parse_failed;
		*eol = '\0';
		if (eol[-1] == '\r')
			eol[-1] = '\0';

		if (git_sortedcache_upsert((void **)&ref, backend->refcache, scan) < 0)
			goto parse_failed;
		scan = eol + 1;

		git_oid_cpy(&ref->oid, &oid);

		/* look for optional "^<OID>\n" */

		if (*scan == '^') {
			if (git_oid__fromstr(&oid, scan + 1, backend->oid_type) < 0)
				goto parse_failed;
			scan += oid_hexsize + 1;

			if (scan < eof) {
				if (!(eol = strchr(scan, '\n')))
					goto parse_failed;
				scan = eol + 1;
			}

			git_oid_cpy(&ref->peel, &oid);
			ref->flags |= PACKREF_HAS_PEEL;
		}
		else if (backend->peeling_mode == PEELING_FULL ||
				(backend->peeling_mode == PEELING_STANDARD &&
				 git__prefixcmp(ref->name, GIT_REFS_TAGS_DIR) == 0))
			ref->flags |= PACKREF_CANNOT_PEEL;
	}

	git_sortedcache_wunlock(backend->refcache);
	git_str_dispose(&packedrefs);

	return 0;

parse_failed:
	git_error_set(GIT_ERROR_REFERENCE, "corrupted packed references file");

	GIT_UNUSED(git_sortedcache_clear(backend->refcache, false));
	git_sortedcache_wunlock(backend->refcache);
	git_str_dispose(&packedrefs);

	return -1;
}

static int loose_parse_oid(
	git_oid *oid,
	const char *filename,
	git_str *file_content,
	git_oid_t oid_type)
{
	const char *str = git_str_cstr(file_content);
	size_t oid_hexsize = git_oid_hexsize(oid_type);

	if (git_str_len(file_content) < oid_hexsize)
		goto corrupted;

	/* we need to get 40 OID characters from the file */
	if (git_oid__fromstr(oid, str, oid_type) < 0)
		goto corrupted;

	/* If the file is longer than 40 chars, the 41st must be a space */
	str += oid_hexsize;
	if (*str == '\0' || git__isspace(*str))
		return 0;

corrupted:
	git_error_set(GIT_ERROR_REFERENCE, "corrupted loose reference file: %s", filename);
	return -1;
}

static int loose_readbuffer(git_str *buf, const char *base, const char *path)
{
	int error;

	if ((error = loose_path(buf, base, path)) < 0 ||
	    (error = git_futils_readbuffer(buf, buf->ptr)) < 0)
		git_str_dispose(buf);

	return error;
}

static int loose_lookup_to_packfile(refdb_fs_backend *backend, const char *name)
{
	int error = 0;
	git_str ref_file = GIT_STR_INIT;
	struct packref *ref = NULL;
	git_oid oid;

	/* if we fail to load the loose reference, assume someone changed
	 * the filesystem under us and skip it...
	 */
	if (loose_readbuffer(&ref_file, backend->gitpath, name) < 0) {
		git_error_clear();
		goto done;
	}

	/* skip symbolic refs */
	if (!git__prefixcmp(git_str_cstr(&ref_file), GIT_SYMREF))
		goto done;

	/* parse OID from file */
	if ((error = loose_parse_oid(&oid, name, &ref_file, backend->oid_type)) < 0)
		goto done;

	if ((error = git_sortedcache_wlock(backend->refcache)) < 0)
		goto done;

	if (!(error = git_sortedcache_upsert(
			(void **)&ref, backend->refcache, name))) {

		git_oid_cpy(&ref->oid, &oid);
		ref->flags = PACKREF_WAS_LOOSE;
	}

	git_sortedcache_wunlock(backend->refcache);

done:
	git_str_dispose(&ref_file);
	return error;
}

static int _dirent_loose_load(void *payload, git_str *full_path)
{
	refdb_fs_backend *backend = payload;
	const char *file_path;

	if (git__suffixcmp(full_path->ptr, ".lock") == 0)
		return 0;

	if (git_fs_path_isdir(full_path->ptr)) {
		int error = git_fs_path_direach(
			full_path, backend->direach_flags, _dirent_loose_load, backend);
		/* Race with the filesystem, ignore it */
		if (error == GIT_ENOTFOUND) {
			git_error_clear();
			return 0;
		}

		return error;
	}

	file_path = full_path->ptr + strlen(backend->gitpath);

	return loose_lookup_to_packfile(backend, file_path);
}

/*
 * Load all the loose references from the repository
 * into the in-memory Packfile, and build a vector with
 * all the references so it can be written back to
 * disk.
 */
static int packed_loadloose(refdb_fs_backend *backend)
{
	int error;
	git_str refs_path = GIT_STR_INIT;

	if (git_str_joinpath(&refs_path, backend->gitpath, GIT_REFS_DIR) < 0)
		return -1;

	/*
	 * Load all the loose files from disk into the Packfile table.
	 * This will overwrite any old packed entries with their
	 * updated loose versions
	 */
	error = git_fs_path_direach(
		&refs_path, backend->direach_flags, _dirent_loose_load, backend);

	git_str_dispose(&refs_path);

	return error;
}

static int refdb_fs_backend__exists(
	int *exists,
	git_refdb_backend *_backend,
	const char *ref_name)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	git_str ref_path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(backend);

	*exists = 0;

	if ((error = loose_path(&ref_path, backend->gitpath, ref_name)) < 0)
		goto out;

	if (git_fs_path_isfile(ref_path.ptr)) {
		*exists = 1;
		goto out;
	}

	if ((error = packed_reload(backend)) < 0)
		goto out;

	if (git_sortedcache_lookup(backend->refcache, ref_name) != NULL) {
		*exists = 1;
		goto out;
	}

out:
	git_str_dispose(&ref_path);
	return error;
}

static const char *loose_parse_symbolic(git_str *file_content)
{
	const unsigned int header_len = (unsigned int)strlen(GIT_SYMREF);
	const char *refname_start;

	refname_start = (const char *)file_content->ptr;

	if (git_str_len(file_content) < header_len + 1) {
		git_error_set(GIT_ERROR_REFERENCE, "corrupted loose reference file");
		return NULL;
	}

	/*
	 * Assume we have already checked for the header
	 * before calling this function
	 */
	refname_start += header_len;

	return refname_start;
}

/*
 * Returns whether a reference is stored per worktree or not.
 * Per-worktree references are:
 *
 * - all pseudorefs, e.g. HEAD and MERGE_HEAD
 * - all references stored inside of "refs/bisect/"
 */
static bool is_per_worktree_ref(const char *ref_name)
{
	return git__prefixcmp(ref_name, "refs/") != 0 ||
	       git__prefixcmp(ref_name, "refs/bisect/") == 0 ||
	       git__prefixcmp(ref_name, "refs/worktree/") == 0 ||
	       git__prefixcmp(ref_name, "refs/rewritten/") == 0;
}

static int loose_lookup(
	git_reference **out,
	refdb_fs_backend *backend,
	const char *ref_name)
{
	git_str ref_file = GIT_STR_INIT;
	int error = 0;
	const char *ref_dir;

	if (out)
		*out = NULL;

	if (is_per_worktree_ref(ref_name))
		ref_dir = backend->gitpath;
	else
		ref_dir = backend->commonpath;

	if ((error = loose_readbuffer(&ref_file, ref_dir, ref_name)) < 0)
		/* cannot read loose ref file - gah */;
	else if (git__prefixcmp(git_str_cstr(&ref_file), GIT_SYMREF) == 0) {
		const char *target;

		git_str_rtrim(&ref_file);

		if (!(target = loose_parse_symbolic(&ref_file)))
			error = -1;
		else if (out != NULL)
			*out = git_reference__alloc_symbolic(ref_name, target);
	} else {
		git_oid oid;

		if (!(error = loose_parse_oid(&oid, ref_name, &ref_file, backend->oid_type)) &&
			out != NULL)
			*out = git_reference__alloc(ref_name, &oid, NULL);
	}

	git_str_dispose(&ref_file);
	return error;
}

static int ref_error_notfound(const char *name)
{
	git_error_set(GIT_ERROR_REFERENCE, "reference '%s' not found", name);
	return GIT_ENOTFOUND;
}

static char *packed_set_peeling_mode(
        char *data,
        size_t data_sz,
        refdb_fs_backend *backend)
{
	static const char *traits_header = "# pack-refs with:";
	char *eol;
	backend->peeling_mode = PEELING_NONE;

	if (git__prefixncmp(data, data_sz, traits_header) == 0) {
		size_t hdr_sz = strlen(traits_header);
		const char *sorted = " sorted ";
		const char *peeled = " peeled ";
		const char *fully_peeled = " fully-peeled ";
		data += hdr_sz;
		data_sz -= hdr_sz;

		eol = memchr(data, '\n', data_sz);

		if (!eol)
			return NULL;

		if (git__memmem(data, eol - data, fully_peeled, strlen(fully_peeled)))
			backend->peeling_mode = PEELING_FULL;
		else if (git__memmem(data, eol - data, peeled, strlen(peeled)))
			backend->peeling_mode = PEELING_STANDARD;

		backend->sorted = NULL != git__memmem(data, eol - data, sorted, strlen(sorted));

		return eol + 1;
	}
	return data;
}

static void packed_map_free(refdb_fs_backend *backend)
{
	if (backend->packed_refs_map.data) {
#ifdef GIT_WIN32
		git__free(backend->packed_refs_map.data);
#else
		git_futils_mmap_free(&backend->packed_refs_map);
#endif
		backend->packed_refs_map.data = NULL;
		backend->packed_refs_map.len = 0;
		git_futils_filestamp_set(&backend->packed_refs_stamp, NULL);
	}
}

static int packed_map_check(refdb_fs_backend *backend)
{
	int error = 0;
	git_file fd = -1;
	struct stat st;

	if ((error = git_mutex_lock(&backend->prlock)) < 0)
		return error;

	if (backend->packed_refs_map.data &&
	    !git_futils_filestamp_check(
	            &backend->packed_refs_stamp, backend->refcache->path)) {
		git_mutex_unlock(&backend->prlock);
		return error;
	}
	packed_map_free(backend);

	fd = git_futils_open_ro(backend->refcache->path);
	if (fd < 0) {
		git_mutex_unlock(&backend->prlock);
		if (fd == GIT_ENOTFOUND) {
			git_error_clear();
			return 0;
		}
		return fd;
	}

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		git_mutex_unlock(&backend->prlock);
		git_error_set(GIT_ERROR_OS, "unable to stat packed-refs '%s'", backend->refcache->path);
		return -1;
	}

	if (st.st_size == 0) {
		p_close(fd);
		git_mutex_unlock(&backend->prlock);
		return 0;
	}

	git_futils_filestamp_set_from_stat(&backend->packed_refs_stamp, &st);

#ifdef GIT_WIN32
	/* on windows, we copy the entire file into memory rather than using
	 * mmap() because using mmap() on windows also locks the file and this
	 * map is long-lived. */
	backend->packed_refs_map.len = (size_t)st.st_size;
	backend->packed_refs_map.data =
	        git__malloc(backend->packed_refs_map.len);
	GIT_ERROR_CHECK_ALLOC(backend->packed_refs_map.data);
	{
		ssize_t bytesread =
		        p_read(fd, backend->packed_refs_map.data,
		               backend->packed_refs_map.len);
		error = (bytesread == (ssize_t)backend->packed_refs_map.len) ?  0 : -1;
	}
#else
	error = git_futils_mmap_ro(&backend->packed_refs_map, fd, 0, (size_t)st.st_size);
#endif
	p_close(fd);
	if (error < 0) {
		git_mutex_unlock(&backend->prlock);
		return error;
	}

	packed_set_peeling_mode(
	        backend->packed_refs_map.data, backend->packed_refs_map.len,
	        backend);

	git_mutex_unlock(&backend->prlock);
	return error;
}

/*
 * Find beginning of packed-ref record pointed to by p.
 *   buf - a lower-bound pointer to some memory buffer
 *   p - an upper-bound pointer to the same memory buffer
 */
static const char *start_of_record(const char *buf, const char *p)
{
	const char *nl = p;
	while (true) {
		nl = git__memrchr(buf, '\n', nl - buf);
		if (!nl)
			return buf;

		if (nl[1] == '^' && nl > buf)
			--nl;
		else
			break;
	};
	return nl + 1;
}

/*
 * Find end of packed-ref record pointed to by p.
 *   end - an upper-bound pointer to some memory buffer
 *   p - a lower-bound pointer to the same memory buffer
 */
static const char *end_of_record(const char *p, const char *end)
{
	while (1) {
		size_t sz = end - p;
		p = memchr(p, '\n', sz);
		if (!p)
			return end;
		++p;
		if (p < end && p[0] == '^')
			++p;
		else
			break;
	}
	return p;
}

static int cmp_record_to_refname(
	const char *rec,
	size_t data_end,
	const char *ref_name,
	git_oid_t oid_type)
{
	const size_t ref_len = strlen(ref_name);
	int cmp_val;
	const char *end;
	size_t oid_hexsize = git_oid_hexsize(oid_type);

	rec += oid_hexsize + 1; /* <oid> + space */

	/* an incomplete (corrupt) record is treated as less than ref_name */
	if (data_end < oid_hexsize + 3)
		return -1;

	data_end -= oid_hexsize + 1;

	end = memchr(rec, '\n', data_end);
	if (end)
		data_end = end - rec;

	cmp_val = memcmp(rec, ref_name, min(ref_len, data_end));

	if (cmp_val == 0 && data_end != ref_len)
		return (data_end > ref_len) ? 1 : -1;
	return cmp_val;
}

static int packed_unsorted_lookup(
        git_reference **out,
        refdb_fs_backend *backend,
        const char *ref_name)
{
	int error = 0;
	struct packref *entry;

	if ((error = packed_reload(backend)) < 0)
		return error;

	if (git_sortedcache_rlock(backend->refcache) < 0)
		return -1;

	entry = git_sortedcache_lookup(backend->refcache, ref_name);
	if (!entry) {
		error = ref_error_notfound(ref_name);
	} else {
		*out = git_reference__alloc(ref_name, &entry->oid, &entry->peel);
		if (!*out)
			error = -1;
	}

	git_sortedcache_runlock(backend->refcache);

	return error;
}

static int packed_lookup(
        git_reference **out,
        refdb_fs_backend *backend,
        const char *ref_name)
{
	int error = 0;
	const char *left, *right, *data_end;
	size_t oid_hexsize = git_oid_hexsize(backend->oid_type);

	if ((error = packed_map_check(backend)) < 0)
		return error;

	if (!backend->sorted)
		return packed_unsorted_lookup(out, backend, ref_name);

	left = backend->packed_refs_map.data;
	right = data_end = (const char *) backend->packed_refs_map.data +
	                   backend->packed_refs_map.len;

	while (left < right && *left == '#') {
		if (!(left = memchr(left, '\n', data_end - left)))
			goto parse_failed;
		left++;
	}

	while (left < right) {
		const char *mid, *rec;
		int compare;

		mid = left + (right - left) / 2;
		rec = start_of_record(left, mid);
		compare = cmp_record_to_refname(rec, data_end - rec, ref_name, backend->oid_type);

		if (compare < 0) {
			left = end_of_record(mid, right);
		} else if (compare > 0) {
			right = rec;
		} else {
			const char *eol;
			git_oid oid, peel, *peel_ptr = NULL;

			if (data_end - rec < (long)oid_hexsize ||
			    git_oid__fromstr(&oid, rec, backend->oid_type) < 0) {
				goto parse_failed;
			}
			rec += oid_hexsize + 1;
			if (!(eol = memchr(rec, '\n', data_end - rec))) {
				goto parse_failed;
			}

			/* look for optional "^<OID>\n" */

			if (eol + 1 < data_end) {
				rec = eol + 1;

				if (*rec == '^') {
					rec++;
					if (data_end - rec < (long)oid_hexsize ||
					    git_oid__fromstr(&peel, rec, backend->oid_type) < 0) {
						goto parse_failed;
					}
					peel_ptr = &peel;
				}
			}

			*out = git_reference__alloc(ref_name, &oid, peel_ptr);
			if (!*out) {
				return -1;
			}

			return 0;
		}
	}
	return ref_error_notfound(ref_name);

parse_failed:
	git_error_set(GIT_ERROR_REFERENCE, "corrupted packed references file");
	return -1;
}

static int refdb_fs_backend__lookup(
	git_reference **out,
	git_refdb_backend *_backend,
	const char *ref_name)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	int error;

	GIT_ASSERT_ARG(backend);

	if (!(error = loose_lookup(out, backend, ref_name)))
		return 0;

	/* only try to lookup this reference on the packfile if it
	 * wasn't found on the loose refs; not if there was a critical error */
	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		error = packed_lookup(out, backend, ref_name);
	}
	return error;
}

typedef struct {
	git_reference_iterator parent;

	char *glob;

	git_pool pool;
	git_vector loose;

	git_sortedcache *cache;
	size_t loose_pos;
	size_t packed_pos;
} refdb_fs_iter;

static void refdb_fs_backend__iterator_free(git_reference_iterator *_iter)
{
	refdb_fs_iter *iter = GIT_CONTAINER_OF(_iter, refdb_fs_iter, parent);

	git_vector_dispose(&iter->loose);
	git_pool_clear(&iter->pool);
	git_sortedcache_free(iter->cache);
	git__free(iter);
}

struct iter_load_context {
	refdb_fs_backend *backend;
	refdb_fs_iter *iter;

	/*
	 * If we have a glob with a prefix (eg `refs/heads/ *`) then we can
	 * optimize our prefix to avoid walking refs that we know won't
	 * match. This is that prefix.
	 */
	const char *ref_prefix;
	size_t ref_prefix_len;

	/* Temporary variables to avoid unnecessary allocations */
	git_str ref_name;
	git_str path;
};

static void iter_load_optimize_prefix(struct iter_load_context *ctx)
{
	const char *pos, *last_sep = NULL;

	if (!ctx->iter->glob)
		return;

	for (pos = ctx->iter->glob; *pos; pos++) {
		switch (*pos) {
		case '?':
		case '*':
		case '[':
		case '\\':
			break;
		case '/':
			last_sep = pos;
			/* FALLTHROUGH */
		default:
			continue;
		}
		break;
	}

	if (last_sep) {
		ctx->ref_prefix = ctx->iter->glob;
		ctx->ref_prefix_len = (last_sep - ctx->ref_prefix) + 1;
	}
}

static int iter_load_paths(
	struct iter_load_context *ctx,
	const char *root_path,
	bool worktree)
{
	git_iterator *fsit = NULL;
	git_iterator_options fsit_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	int error = 0;

	fsit_opts.flags = ctx->backend->iterator_flags;

	git_str_clear(&ctx->path);
	git_str_puts(&ctx->path, root_path);
	git_str_put(&ctx->path, ctx->ref_prefix, ctx->ref_prefix_len);

	fsit_opts.flags = ctx->backend->iterator_flags;
	fsit_opts.oid_type = ctx->backend->oid_type;

	if ((error = git_iterator_for_filesystem(&fsit, ctx->path.ptr, &fsit_opts)) < 0) {
		/*
		 * Subdirectories - either glob provided or per-worktree refs - need
		 * not exist.
		 */
		if ((worktree || ctx->iter->glob) && error == GIT_ENOTFOUND)
			error = 0;

		goto done;
	}

	git_str_clear(&ctx->ref_name);
	git_str_put(&ctx->ref_name, ctx->ref_prefix, ctx->ref_prefix_len);

	while (git_iterator_advance(&entry, fsit) == 0) {
		char *ref_dup;

		git_str_truncate(&ctx->ref_name, ctx->ref_prefix_len);
		git_str_puts(&ctx->ref_name, entry->path);

		if (worktree) {
			if (!is_per_worktree_ref(ctx->ref_name.ptr))
				continue;
		} else {
			if (git_repository_is_worktree(ctx->backend->repo) &&
			    is_per_worktree_ref(ctx->ref_name.ptr))
				continue;
		}

		if (git__suffixcmp(ctx->ref_name.ptr, ".lock") == 0)
			continue;

		if (ctx->iter->glob && wildmatch(ctx->iter->glob, ctx->ref_name.ptr, 0))
			continue;

		ref_dup = git_pool_strdup(&ctx->iter->pool, ctx->ref_name.ptr);
		GIT_ERROR_CHECK_ALLOC(ref_dup);

		if ((error = git_vector_insert(&ctx->iter->loose, ref_dup)) < 0)
			goto done;
	}

done:
	git_iterator_free(fsit);
	return error;
}

#define iter_load_context_init(b, i) { b, i, GIT_REFS_DIR, CONST_STRLEN(GIT_REFS_DIR) }
#define iter_load_context_dispose(ctx) do {  \
	git_str_dispose(&((ctx)->path));     \
	git_str_dispose(&((ctx)->ref_name)); \
} while(0)

static int iter_load_loose_paths(
	refdb_fs_backend *backend,
	refdb_fs_iter *iter)
{
	struct iter_load_context ctx = iter_load_context_init(backend, iter);

	int error = 0;

	if (!backend->commonpath)
		return 0;

	iter_load_optimize_prefix(&ctx);

	if ((error = iter_load_paths(&ctx,
			backend->commonpath, false)) < 0)
		goto done;

	if (git_repository_is_worktree(backend->repo)) {
		if ((error = iter_load_paths(&ctx,
				backend->gitpath, true)) < 0)
			goto done;
	}

done:
	iter_load_context_dispose(&ctx);
	return error;
}

static int refdb_fs_backend__iterator_next(
	git_reference **out, git_reference_iterator *_iter)
{
	int error = GIT_ITEROVER;
	refdb_fs_iter *iter = GIT_CONTAINER_OF(_iter, refdb_fs_iter, parent);
	refdb_fs_backend *backend = GIT_CONTAINER_OF(iter->parent.db->backend, refdb_fs_backend, parent);
	struct packref *ref;

	while (iter->loose_pos < iter->loose.length) {
		const char *path = git_vector_get(&iter->loose, iter->loose_pos++);

		if (loose_lookup(out, backend, path) == 0) {
			ref = git_sortedcache_lookup(iter->cache, path);
			if (ref)
				ref->flags |= PACKREF_SHADOWED;

			return 0;
		}

		git_error_clear();
	}

	error = GIT_ITEROVER;
	while (iter->packed_pos < git_sortedcache_entrycount(iter->cache)) {
		ref = git_sortedcache_entry(iter->cache, iter->packed_pos++);
		if (!ref) /* stop now if another thread deleted refs and we past end */
			break;

		if (ref->flags & PACKREF_SHADOWED)
			continue;
		if (iter->glob && wildmatch(iter->glob, ref->name, 0) != 0)
			continue;

		*out = git_reference__alloc(ref->name, &ref->oid, &ref->peel);
		error = (*out != NULL) ? 0 : -1;
		break;
	}

	return error;
}

static int refdb_fs_backend__iterator_next_name(
	const char **out, git_reference_iterator *_iter)
{
	int error = GIT_ITEROVER;
	refdb_fs_iter *iter = GIT_CONTAINER_OF(_iter, refdb_fs_iter, parent);
	refdb_fs_backend *backend = GIT_CONTAINER_OF(iter->parent.db->backend, refdb_fs_backend, parent);
	struct packref *ref;

	while (iter->loose_pos < iter->loose.length) {
		const char *path = git_vector_get(&iter->loose, iter->loose_pos++);
		struct packref *ref;

		if (loose_lookup(NULL, backend, path) == 0) {
			ref = git_sortedcache_lookup(iter->cache, path);
			if (ref)
				ref->flags |= PACKREF_SHADOWED;

			*out = path;
			return 0;
		}

		git_error_clear();
	}

	error = GIT_ITEROVER;
	while (iter->packed_pos < git_sortedcache_entrycount(iter->cache)) {
		ref = git_sortedcache_entry(iter->cache, iter->packed_pos++);
		if (!ref) /* stop now if another thread deleted refs and we past end */
			break;

		if (ref->flags & PACKREF_SHADOWED)
			continue;
		if (iter->glob && wildmatch(iter->glob, ref->name, 0) != 0)
			continue;

		*out = ref->name;
		error = 0;
		break;
	}

	return error;
}

static int refdb_fs_backend__iterator(
	git_reference_iterator **out, git_refdb_backend *_backend, const char *glob)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	refdb_fs_iter *iter = NULL;
	int error;

	GIT_ASSERT_ARG(backend);

	iter = git__calloc(1, sizeof(refdb_fs_iter));
	GIT_ERROR_CHECK_ALLOC(iter);

	if ((error = git_pool_init(&iter->pool, 1)) < 0)
		goto out;

	if ((error = git_vector_init(&iter->loose, 8, NULL)) < 0)
		goto out;

	if (glob != NULL &&
	    (iter->glob = git_pool_strdup(&iter->pool, glob)) == NULL) {
		error = GIT_ERROR_NOMEMORY;
		goto out;
	}

	if ((error = iter_load_loose_paths(backend, iter)) < 0)
		goto out;

	if ((error = packed_reload(backend)) < 0)
		goto out;

	if ((error = git_sortedcache_copy(&iter->cache, backend->refcache, 1, NULL, NULL)) < 0)
		goto out;

	iter->parent.next = refdb_fs_backend__iterator_next;
	iter->parent.next_name = refdb_fs_backend__iterator_next_name;
	iter->parent.free = refdb_fs_backend__iterator_free;

	*out = (git_reference_iterator *)iter;
out:
	if (error)
		refdb_fs_backend__iterator_free((git_reference_iterator *)iter);
	return error;
}

static bool ref_is_available(
	const char *old_ref, const char *new_ref, const char *this_ref)
{
	if (old_ref == NULL || strcmp(old_ref, this_ref)) {
		size_t reflen = strlen(this_ref);
		size_t newlen = strlen(new_ref);
		size_t cmplen = reflen < newlen ? reflen : newlen;
		const char *lead = reflen < newlen ? new_ref : this_ref;

		if (!strncmp(new_ref, this_ref, cmplen) && lead[cmplen] == '/') {
			return false;
		}
	}

	return true;
}

static int reference_path_available(
	refdb_fs_backend *backend,
	const char *new_ref,
	const char *old_ref,
	int force)
{
	size_t i;
	int error;

	if ((error = packed_reload(backend)) < 0)
		return error;

	if (!force) {
		int exists;

		if ((error = refdb_fs_backend__exists(
			&exists, (git_refdb_backend *)backend, new_ref)) < 0) {
			return error;
		}

		if (exists) {
			git_error_set(GIT_ERROR_REFERENCE,
				"failed to write reference '%s': a reference with "
				"that name already exists.", new_ref);
			return GIT_EEXISTS;
		}
	}

	if ((error = git_sortedcache_rlock(backend->refcache)) < 0)
		return error;

	for (i = 0; i < git_sortedcache_entrycount(backend->refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(backend->refcache, i);

		if (ref && !ref_is_available(old_ref, new_ref, ref->name)) {
			git_sortedcache_runlock(backend->refcache);
			git_error_set(GIT_ERROR_REFERENCE,
				"path to reference '%s' collides with existing one", new_ref);
			return -1;
		}
	}

	git_sortedcache_runlock(backend->refcache);
	return 0;
}

static int loose_lock(git_filebuf *file, refdb_fs_backend *backend, const char *name)
{
	int error, filebuf_flags;
	git_str ref_path = GIT_STR_INIT;
	const char *basedir;

	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(name);

	if (!git_path_is_valid(backend->repo, name, 0, GIT_FS_PATH_REJECT_FILESYSTEM_DEFAULTS)) {
		git_error_set(GIT_ERROR_INVALID, "invalid reference name '%s'", name);
		return GIT_EINVALIDSPEC;
	}

	if (is_per_worktree_ref(name))
		basedir = backend->gitpath;
	else
		basedir = backend->commonpath;

	/* Remove a possibly existing empty directory hierarchy
	 * which name would collide with the reference name
	 */
	if ((error = git_futils_rmdir_r(name, basedir, GIT_RMDIR_SKIP_NONEMPTY)) < 0)
		return error;

	if ((error = loose_path(&ref_path, basedir, name)) < 0)
		return error;

	filebuf_flags = GIT_FILEBUF_CREATE_LEADING_DIRS;
	if (backend->fsync)
		filebuf_flags |= GIT_FILEBUF_FSYNC;

	error = git_filebuf_open(file, ref_path.ptr, filebuf_flags, GIT_REFS_FILE_MODE);

	if (error == GIT_EDIRECTORY)
		git_error_set(GIT_ERROR_REFERENCE, "cannot lock ref '%s', there are refs beneath that folder", name);

	git_str_dispose(&ref_path);
	return error;
}

static int loose_commit(git_filebuf *file, const git_reference *ref)
{
	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(ref);

	if (ref->type == GIT_REFERENCE_DIRECT) {
		char oid[GIT_OID_MAX_HEXSIZE + 1];
		git_oid_nfmt(oid, sizeof(oid), &ref->target.oid);

		git_filebuf_printf(file, "%s\n", oid);
	} else if (ref->type == GIT_REFERENCE_SYMBOLIC) {
		git_filebuf_printf(file, GIT_SYMREF "%s\n", ref->target.symbolic);
	} else {
		GIT_ASSERT(0);
	}

	return git_filebuf_commit(file);
}

static int refdb_fs_backend__lock(void **out, git_refdb_backend *_backend, const char *refname)
{
	int error;
	git_filebuf *lock;
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);

	lock = git__calloc(1, sizeof(git_filebuf));
	GIT_ERROR_CHECK_ALLOC(lock);

	if ((error = loose_lock(lock, backend, refname)) < 0) {
		git__free(lock);
		return error;
	}

	*out = lock;
	return 0;
}

static int refdb_fs_backend__write_tail(
	git_refdb_backend *_backend,
	const git_reference *ref,
	git_filebuf *file,
	int update_reflog,
	const git_oid *old_id,
	const char *old_target,
	const git_signature *who,
	const char *message);

static int refdb_fs_backend__delete_tail(
	git_refdb_backend *_backend,
	git_filebuf *file,
	const char *ref_name,
	const git_oid *old_id,
	const char *old_target);

static int refdb_fs_backend__unlock(git_refdb_backend *backend, void *payload, int success, int update_reflog,
				    const git_reference *ref, const git_signature *sig, const char *message)
{
	git_filebuf *lock = (git_filebuf *) payload;
	int error = 0;

	if (success == 2)
		error = refdb_fs_backend__delete_tail(backend, lock, ref->name, NULL, NULL);
	else if (success)
		error = refdb_fs_backend__write_tail(backend, ref, lock, update_reflog, NULL, NULL, sig, message);
	else
		git_filebuf_cleanup(lock);

	git__free(lock);
	return error;
}

/*
 * Find out what object this reference resolves to.
 *
 * For references that point to a 'big' tag (e.g. an
 * actual tag object on the repository), we need to
 * cache on the packfile the OID of the object to
 * which that 'big tag' is pointing to.
 */
static int packed_find_peel(refdb_fs_backend *backend, struct packref *ref)
{
	git_object *object;

	if (ref->flags & PACKREF_HAS_PEEL || ref->flags & PACKREF_CANNOT_PEEL)
		return 0;

	/*
	 * Find the tagged object in the repository
	 */
	if (git_object_lookup(&object, backend->repo, &ref->oid, GIT_OBJECT_ANY) < 0)
		return -1;

	/*
	 * If the tagged object is a Tag object, we need to resolve it;
	 * if the ref is actually a 'weak' ref, we don't need to resolve
	 * anything.
	 */
	if (git_object_type(object) == GIT_OBJECT_TAG) {
		git_tag *tag = (git_tag *)object;

		/*
		 * Find the object pointed at by this tag
		 */
		git_oid_cpy(&ref->peel, git_tag_target_id(tag));
		ref->flags |= PACKREF_HAS_PEEL;

		/*
		 * The reference has now cached the resolved OID, and is
		 * marked at such. When written to the packfile, it'll be
		 * accompanied by this resolved oid
		 */
	}

	git_object_free(object);
	return 0;
}

/*
 * Write a single reference into a packfile
 */
static int packed_write_ref(struct packref *ref, git_filebuf *file)
{
	char oid[GIT_OID_MAX_HEXSIZE + 1];
	git_oid_nfmt(oid, sizeof(oid), &ref->oid);

	/*
	 * For references that peel to an object in the repo, we must
	 * write the resulting peel on a separate line, e.g.
	 *
	 *	6fa8a902cc1d18527e1355773c86721945475d37 refs/tags/libgit2-0.4
	 *	^2ec0cb7959b0bf965d54f95453f5b4b34e8d3100
	 *
	 * This obviously only applies to tags.
	 * The required peels have already been loaded into `ref->peel_target`.
	 */
	if (ref->flags & PACKREF_HAS_PEEL) {
		char peel[GIT_OID_MAX_HEXSIZE + 1];
		git_oid_nfmt(peel, sizeof(peel), &ref->peel);

		if (git_filebuf_printf(file, "%s %s\n^%s\n", oid, ref->name, peel) < 0)
			return -1;
	} else {
		if (git_filebuf_printf(file, "%s %s\n", oid, ref->name) < 0)
			return -1;
	}

	return 0;
}

/*
 * Remove all loose references
 *
 * Once we have successfully written a packfile,
 * all the loose references that were packed must be
 * removed from disk.
 *
 * This is a dangerous method; make sure the packfile
 * is well-written, because we are destructing references
 * here otherwise.
 */
static int packed_remove_loose(refdb_fs_backend *backend)
{
	size_t i;
	git_filebuf lock = GIT_FILEBUF_INIT;
	git_str ref_content = GIT_STR_INIT;
	int error = 0;

	/* backend->refcache is already locked when this is called */

	for (i = 0; i < git_sortedcache_entrycount(backend->refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(backend->refcache, i);
		git_oid current_id;

		if (!ref || !(ref->flags & PACKREF_WAS_LOOSE))
			continue;

		git_filebuf_cleanup(&lock);

		/* We need to stop anybody from updating the ref while we try to do a safe delete */
		error = loose_lock(&lock, backend, ref->name);
		/* If someone else is updating it, let them do it */
		if (error == GIT_EEXISTS || error == GIT_ENOTFOUND)
			continue;

		if (error < 0) {
			git_str_dispose(&ref_content);
			git_error_set(GIT_ERROR_REFERENCE, "failed to lock loose reference '%s'", ref->name);
			return error;
		}

		error = git_futils_readbuffer(&ref_content, lock.path_original);
		/* Someone else beat us to cleaning up the ref, let's simply continue */
		if (error == GIT_ENOTFOUND)
			continue;

		/* This became a symref between us packing and trying to delete it, so ignore it */
		if (!git__prefixcmp(ref_content.ptr, GIT_SYMREF))
			continue;

		/* Figure out the current id; if we find a bad ref file, skip it so we can do the rest */
		if (loose_parse_oid(&current_id, lock.path_original, &ref_content, backend->oid_type) < 0)
			continue;

		/* If the ref moved since we packed it, we must not delete it */
		if (!git_oid_equal(&current_id, &ref->oid))
			continue;

		/*
		 * if we fail to remove a single file, this is *not* good,
		 * but we should keep going and remove as many as possible.
		 * If we fail to remove, the ref is still in the old state, so
		 * we haven't lost information.
		 */
		p_unlink(lock.path_original);
	}

	git_str_dispose(&ref_content);
	git_filebuf_cleanup(&lock);
	return 0;
}

/*
 * Write all the contents in the in-memory packfile to disk.
 */
static int packed_write(refdb_fs_backend *backend)
{
	git_sortedcache *refcache = backend->refcache;
	git_filebuf pack_file = GIT_FILEBUF_INIT;
	int error, open_flags = 0;
	size_t i;

	/* take lock and close up packed-refs mmap if open */
	if ((error = git_mutex_lock(&backend->prlock)) < 0) {
		return error;
	}

	packed_map_free(backend);

	git_mutex_unlock(&backend->prlock);

	/* lock the cache to updates while we do this */
	if ((error = git_sortedcache_wlock(refcache)) < 0)
		return error;

	if (backend->fsync)
		open_flags = GIT_FILEBUF_FSYNC;

	/* Open the file! */
	if ((error = git_filebuf_open(&pack_file, git_sortedcache_path(refcache), open_flags, GIT_PACKEDREFS_FILE_MODE)) < 0)
		goto fail;

	/* Packfiles have a header... apparently
	 * This is in fact not required, but we might as well print it
	 * just for kicks */
	if ((error = git_filebuf_printf(&pack_file, "%s\n", GIT_PACKEDREFS_HEADER)) < 0)
		goto fail;

	for (i = 0; i < git_sortedcache_entrycount(refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(refcache, i);

		GIT_ASSERT_WITH_CLEANUP(ref, {
			error = -1;
			goto fail;
		});

		if ((error = packed_find_peel(backend, ref)) < 0)
			goto fail;

		if ((error = packed_write_ref(ref, &pack_file)) < 0)
			goto fail;
	}

	/* if we've written all the references properly, we can commit
	 * the packfile to make the changes effective */
	if ((error = git_filebuf_commit(&pack_file)) < 0)
		goto fail;

	/* when and only when the packfile has been properly written,
	 * we can go ahead and remove the loose refs */
	if ((error = packed_remove_loose(backend)) < 0)
		goto fail;

	git_sortedcache_updated(refcache);
	git_sortedcache_wunlock(refcache);

	/* we're good now */
	return 0;

fail:
	git_filebuf_cleanup(&pack_file);
	git_sortedcache_wunlock(refcache);

	return error;
}

static int packed_delete(refdb_fs_backend *backend, const char *ref_name)
{
	size_t pack_pos;
	int error, found = 0;

	if ((error = packed_reload(backend)) < 0)
		goto cleanup;

	if ((error = git_sortedcache_wlock(backend->refcache)) < 0)
		goto cleanup;

	/* If a packed reference exists, remove it from the packfile and repack if necessary */
	error = git_sortedcache_lookup_index(&pack_pos, backend->refcache, ref_name);
	if (error == 0) {
		error = git_sortedcache_remove(backend->refcache, pack_pos);
		found = 1;
	}
	if (error == GIT_ENOTFOUND)
		error = 0;

	git_sortedcache_wunlock(backend->refcache);

	if (found)
		error = packed_write(backend);

cleanup:
	return error;
}

static int reflog_append(refdb_fs_backend *backend, const git_reference *ref, const git_oid *old, const git_oid *new, const git_signature *author, const char *message);

static int cmp_old_ref(int *cmp, git_refdb_backend *backend, const char *name,
	const git_oid *old_id, const char *old_target)
{
	int error = 0;
	git_reference *old_ref = NULL;

	*cmp = 0;
	/* It "matches" if there is no old value to compare against */
	if (!old_id && !old_target)
		return 0;

	if ((error = refdb_fs_backend__lookup(&old_ref, backend, name)) < 0) {
		if (error == GIT_ENOTFOUND && old_id && git_oid_is_zero(old_id))
			return 0;
		goto out;
	}

	/* If the types don't match, there's no way the values do */
	if (old_id && old_ref->type != GIT_REFERENCE_DIRECT) {
		*cmp = -1;
		goto out;
	}
	if (old_target && old_ref->type != GIT_REFERENCE_SYMBOLIC) {
		*cmp = 1;
		goto out;
	}

	if (old_id && old_ref->type == GIT_REFERENCE_DIRECT)
		*cmp = git_oid_cmp(old_id, &old_ref->target.oid);

	if (old_target && old_ref->type == GIT_REFERENCE_SYMBOLIC)
		*cmp = git__strcmp(old_target, old_ref->target.symbolic);

out:
	git_reference_free(old_ref);

	return error;
}

/*
 * The git.git comment regarding this, for your viewing pleasure:
 *
 * Special hack: If a branch is updated directly and HEAD
 * points to it (may happen on the remote side of a push
 * for example) then logically the HEAD reflog should be
 * updated too.
 * A generic solution implies reverse symref information,
 * but finding all symrefs pointing to the given branch
 * would be rather costly for this rare event (the direct
 * update of a branch) to be worth it.  So let's cheat and
 * check with HEAD only which should cover 99% of all usage
 * scenarios (even 100% of the default ones).
 */
static int maybe_append_head(refdb_fs_backend *backend, const git_reference *ref, const git_signature *who, const char *message)
{
	git_reference *head = NULL;
	git_refdb *refdb = NULL;
	int error, write_reflog;
	git_oid old_id;

	if ((error = git_repository_refdb(&refdb, backend->repo)) < 0 ||
	    (error = git_refdb_should_write_head_reflog(&write_reflog, refdb, ref)) < 0)
		goto out;
	if (!write_reflog)
		goto out;

	/* if we can't resolve, we use {0}*40 as old id */
	if (git_reference_name_to_id(&old_id, backend->repo, ref->name) < 0)
		memset(&old_id, 0, sizeof(old_id));

	if ((error = git_reference_lookup(&head, backend->repo, GIT_HEAD_FILE)) < 0 ||
	    (error = reflog_append(backend, head, &old_id, git_reference_target(ref), who, message)) < 0)
		goto out;

out:
	git_reference_free(head);
	git_refdb_free(refdb);
	return error;
}

static int refdb_fs_backend__write(
	git_refdb_backend *_backend,
	const git_reference *ref,
	int force,
	const git_signature *who,
	const char *message,
	const git_oid *old_id,
	const char *old_target)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	git_filebuf file = GIT_FILEBUF_INIT;
	int error = 0;

	GIT_ASSERT_ARG(backend);

	if ((error = reference_path_available(backend, ref->name, NULL, force)) < 0)
		return error;

	/* We need to perform the reflog append and old value check under the ref's lock */
	if ((error = loose_lock(&file, backend, ref->name)) < 0)
		return error;

	return refdb_fs_backend__write_tail(_backend, ref, &file, true, old_id, old_target, who, message);
}

static int refdb_fs_backend__write_tail(
	git_refdb_backend *_backend,
	const git_reference *ref,
	git_filebuf *file,
	int update_reflog,
	const git_oid *old_id,
	const char *old_target,
	const git_signature *who,
	const char *message)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	int error = 0, cmp = 0, should_write;
	const char *new_target = NULL;
	const git_oid *new_id = NULL;

	if ((error = cmp_old_ref(&cmp, _backend, ref->name, old_id, old_target)) < 0)
		goto on_error;

	if (cmp) {
		git_error_set(GIT_ERROR_REFERENCE, "old reference value does not match");
		error = GIT_EMODIFIED;
		goto on_error;
	}

	if (ref->type == GIT_REFERENCE_SYMBOLIC)
		new_target = ref->target.symbolic;
	else
		new_id = &ref->target.oid;

	error = cmp_old_ref(&cmp, _backend, ref->name, new_id, new_target);
	if (error < 0 && error != GIT_ENOTFOUND)
		goto on_error;

	/* Don't update if we have the same value */
	if (!error && !cmp) {
		error = 0;
		goto on_error; /* not really error */
	}

	if (update_reflog) {
		git_refdb *refdb;

		if ((error = git_repository_refdb__weakptr(&refdb, backend->repo)) < 0 ||
		    (error = git_refdb_should_write_reflog(&should_write, refdb, ref)) < 0)
			goto on_error;

		if (should_write) {
			if ((error = reflog_append(backend, ref, NULL, NULL, who, message)) < 0)
				goto on_error;
			if ((error = maybe_append_head(backend, ref, who, message)) < 0)
				goto on_error;
		}
	}

	return loose_commit(file, ref);

on_error:
        git_filebuf_cleanup(file);
        return error;
}

static int refdb_fs_backend__prune_refs(
	refdb_fs_backend *backend,
	const char *ref_name,
	const char *prefix)
{
	git_str relative_path = GIT_STR_INIT;
	git_str base_path = GIT_STR_INIT;
	size_t commonlen;
	int error;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(ref_name);

	if ((error = git_str_sets(&relative_path, ref_name)) < 0)
		goto cleanup;

	git_fs_path_squash_slashes(&relative_path);
	if ((commonlen = git_fs_path_common_dirlen("refs/heads/", git_str_cstr(&relative_path))) == strlen("refs/heads/") ||
		(commonlen = git_fs_path_common_dirlen("refs/tags/", git_str_cstr(&relative_path))) == strlen("refs/tags/") ||
		(commonlen = git_fs_path_common_dirlen("refs/remotes/", git_str_cstr(&relative_path))) == strlen("refs/remotes/")) {

		git_str_truncate(&relative_path, commonlen);

		if (prefix)
			error = git_str_join3(&base_path, '/',
				backend->commonpath, prefix,
				git_str_cstr(&relative_path));
		else
			error = git_str_joinpath(&base_path,
				backend->commonpath,
				git_str_cstr(&relative_path));

		if (!error)
			error = git_path_validate_str_length(NULL, &base_path);

		if (error < 0)
			goto cleanup;

		error = git_futils_rmdir_r(ref_name + commonlen,
			git_str_cstr(&base_path),
			GIT_RMDIR_EMPTY_PARENTS | GIT_RMDIR_SKIP_ROOT);

		if (error == GIT_ENOTFOUND)
			error = 0;
	}

cleanup:
	git_str_dispose(&relative_path);
	git_str_dispose(&base_path);
	return error;
}

static int refdb_fs_backend__delete(
	git_refdb_backend *_backend,
	const char *ref_name,
	const git_oid *old_id, const char *old_target)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	git_filebuf file = GIT_FILEBUF_INIT;
	int error = 0;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(ref_name);

	if ((error = loose_lock(&file, backend, ref_name)) < 0)
		return error;

	if ((error = refdb_reflog_fs__delete(_backend, ref_name)) < 0) {
		git_filebuf_cleanup(&file);
		return error;
	}

	return refdb_fs_backend__delete_tail(_backend, &file, ref_name, old_id, old_target);
}

static int loose_delete(refdb_fs_backend *backend, const char *ref_name)
{
	git_str path = GIT_STR_INIT;
	int error = 0;

	if ((error = loose_path(&path, backend->commonpath, ref_name)) < 0)
		return error;

	error = p_unlink(path.ptr);
	if (error < 0 && errno == ENOENT)
		error = GIT_ENOTFOUND;
	else if (error != 0)
		error = -1;

	git_str_dispose(&path);

	return error;
}

static int refdb_fs_backend__delete_tail(
	git_refdb_backend *_backend,
	git_filebuf *file,
	const char *ref_name,
	const git_oid *old_id, const char *old_target)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	int error = 0, cmp = 0;
	bool packed_deleted = 0;

	error = cmp_old_ref(&cmp, _backend, ref_name, old_id, old_target);
	if (error < 0)
		goto cleanup;

	if (cmp) {
		git_error_set(GIT_ERROR_REFERENCE, "old reference value does not match");
		error = GIT_EMODIFIED;
		goto cleanup;
	}

	/*
	 * To ensure that an external observer will see either the current ref value
	 * (because the loose ref still exists), or a missing ref (after the packed-file is
	 * unlocked, there will be nothing left), we must ensure things happen in the
	 * following order:
	 *
	 * - the packed-ref file is locked and loaded, as well as a loose one, if it exists
	 * - we optimistically delete a packed ref, keeping track of whether it existed
	 * - we delete the loose ref, note that we have its .lock
	 * - the loose ref is "unlocked", then the packed-ref file is rewritten and unlocked
	 * - we should prune the path components if a loose ref was deleted
	 *
	 * Note that, because our packed backend doesn't expose its filesystem lock,
	 * we might not be able to guarantee that this is what actually happens (ie.
	 * as our current code never write packed-refs.lock, nothing stops observers
	 * from grabbing a "stale" value from there).
	 */
	if ((error = packed_delete(backend, ref_name)) < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if (error == 0)
		packed_deleted = 1;

	if ((error = loose_delete(backend, ref_name)) < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if (error == GIT_ENOTFOUND) {
		error = packed_deleted ? 0 : ref_error_notfound(ref_name);
		goto cleanup;
	}

cleanup:
	git_filebuf_cleanup(file);
	if (error == 0)
		error = refdb_fs_backend__prune_refs(backend, ref_name, "");
	return error;
}

static int refdb_reflog_fs__rename(git_refdb_backend *_backend, const char *old_name, const char *new_name);

static int refdb_fs_backend__rename(
	git_reference **out,
	git_refdb_backend *_backend,
	const char *old_name,
	const char *new_name,
	int force,
	const git_signature *who,
	const char *message)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	git_reference *old, *new = NULL;
	git_filebuf file = GIT_FILEBUF_INIT;
	int error;

	GIT_ASSERT_ARG(backend);

	if ((error = reference_path_available(
			backend, new_name, old_name, force)) < 0 ||
		(error = refdb_fs_backend__lookup(&old, _backend, old_name)) < 0)
		return error;

	if ((error = loose_lock(&file, backend, old->name)) < 0) {
		git_reference_free(old);
		return error;
	}

	new = git_reference__realloc(&old, new_name);
	if (!new) {
		git_reference_free(old);
		git_filebuf_cleanup(&file);
		return -1;
	}

	if ((error = refdb_fs_backend__delete_tail(_backend, &file, old_name, NULL, NULL)) < 0) {
		git_reference_free(new);
		git_filebuf_cleanup(&file);
		return error;
	}

	if ((error = loose_lock(&file, backend, new_name)) < 0) {
		git_reference_free(new);
		return error;
	}

	/* Try to rename the refog; it's ok if the old doesn't exist */
	error = refdb_reflog_fs__rename(_backend, old_name, new_name);
	if (((error == 0) || (error == GIT_ENOTFOUND)) &&
		((error = reflog_append(backend, new, git_reference_target(new), NULL, who, message)) < 0)) {
		git_reference_free(new);
		git_filebuf_cleanup(&file);
		return error;
	}

	if ((error = loose_commit(&file, new)) < 0 || out == NULL) {
		git_reference_free(new);
		git_filebuf_cleanup(&file);
		return error;
	}

	*out = new;
	return 0;
}

static int refdb_fs_backend__compress(git_refdb_backend *_backend)
{
	int error;
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);

	GIT_ASSERT_ARG(backend);

	if ((error = packed_reload(backend)) < 0 || /* load the existing packfile */
	    (error = packed_loadloose(backend)) < 0 || /* add all the loose refs */
	    (error = packed_write(backend)) < 0) /* write back to disk */
		return error;

	return 0;
}

static void refdb_fs_backend__free(git_refdb_backend *_backend)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);

	if (!backend)
		return;

	git_sortedcache_free(backend->refcache);

	git_mutex_lock(&backend->prlock);
	packed_map_free(backend);
	git_mutex_unlock(&backend->prlock);
	git_mutex_free(&backend->prlock);

	git__free(backend->gitpath);
	git__free(backend->commonpath);
	git__free(backend);
}

static char *setup_namespace(git_repository *repo, const char *in)
{
	git_str path = GIT_STR_INIT;
	char *parts, *start, *end, *out = NULL;

	if (!in)
		goto done;

	git_str_puts(&path, in);

	/* if the repo is not namespaced, nothing else to do */
	if (repo->namespace == NULL) {
		out = git_str_detach(&path);
		goto done;
	}

	parts = end = git__strdup(repo->namespace);
	if (parts == NULL)
		goto done;

	/*
	 * From `man gitnamespaces`:
	 *  namespaces which include a / will expand to a hierarchy
	 *  of namespaces; for example, GIT_NAMESPACE=foo/bar will store
	 *  refs under refs/namespaces/foo/refs/namespaces/bar/
	 */
	while ((start = git__strsep(&end, "/")) != NULL)
		git_str_printf(&path, "refs/namespaces/%s/", start);

	git_str_printf(&path, "refs/namespaces/%s/refs", end);
	git__free(parts);

	/* Make sure that the folder with the namespace exists */
	if (git_futils_mkdir_relative(git_str_cstr(&path), in, 0777,
			GIT_MKDIR_PATH, NULL) < 0)
		goto done;

	/* Return root of the namespaced gitpath, i.e. without the trailing 'refs' */
	git_str_rtruncate_at_char(&path, '/');
	git_str_putc(&path, '/');
	out = git_str_detach(&path);

done:
	git_str_dispose(&path);
	return out;
}

static int reflog_alloc(
	git_reflog **reflog,
	const char *name,
	git_oid_t oid_type)
{
	git_reflog *log;

	*reflog = NULL;

	log = git__calloc(1, sizeof(git_reflog));
	GIT_ERROR_CHECK_ALLOC(log);

	log->ref_name = git__strdup(name);
	GIT_ERROR_CHECK_ALLOC(log->ref_name);

	log->oid_type = oid_type;

	if (git_vector_init(&log->entries, 0, NULL) < 0) {
		git__free(log->ref_name);
		git__free(log);
		return -1;
	}

	*reflog = log;

	return 0;
}

static int reflog_parse(git_reflog *log, const char *buf, size_t buf_size)
{
	git_parse_ctx parser = GIT_PARSE_CTX_INIT;

	if ((git_parse_ctx_init(&parser, buf, buf_size)) < 0)
		return -1;

	for (; parser.remain_len; git_parse_advance_line(&parser)) {
		git_reflog_entry *entry;
		const char *sig;
		char c;

		entry = git__calloc(1, sizeof(*entry));
		GIT_ERROR_CHECK_ALLOC(entry);
		entry->committer = git__calloc(1, sizeof(*entry->committer));
		GIT_ERROR_CHECK_ALLOC(entry->committer);

		if (git_parse_advance_oid(&entry->oid_old, &parser, log->oid_type) < 0 ||
		    git_parse_advance_expected(&parser, " ", 1) < 0 ||
		    git_parse_advance_oid(&entry->oid_cur, &parser, log->oid_type) < 0)
			goto next;

		sig = parser.line;
		while (git_parse_peek(&c, &parser, 0) == 0 && c != '\t' && c != '\n')
			git_parse_advance_chars(&parser, 1);

		if (git_signature__parse(entry->committer, &sig, parser.line, NULL, 0) < 0)
			goto next;

		if (c == '\t') {
			size_t len;
			git_parse_advance_chars(&parser, 1);

			len = parser.line_len;
			if (parser.line[len - 1] == '\n')
				len--;

			entry->msg = git__strndup(parser.line, len);
			GIT_ERROR_CHECK_ALLOC(entry->msg);
		}

		if ((git_vector_insert(&log->entries, entry)) < 0) {
			git_reflog_entry__free(entry);
			return -1;
		}

		continue;

next:
		git_reflog_entry__free(entry);
	}

	return 0;
}

static int create_new_reflog_file(const char *filepath)
{
	int fd, error;

	if ((error = git_futils_mkpath2file(filepath, GIT_REFLOG_DIR_MODE)) < 0)
		return error;

	if ((fd = p_open(filepath,
			O_WRONLY | O_CREAT,
			GIT_REFLOG_FILE_MODE)) < 0)
		return -1;

	return p_close(fd);
}

static int refdb_reflog_fs__ensure_log(git_refdb_backend *_backend, const char *name)
{
	refdb_fs_backend *backend;
	git_repository *repo;
	git_str path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(_backend && name);

	backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	repo = backend->repo;

	if ((error = reflog_path(&path, repo, name)) < 0)
		return error;

	error = create_new_reflog_file(git_str_cstr(&path));
	git_str_dispose(&path);

	return error;
}

static int has_reflog(git_repository *repo, const char *name)
{
	int ret = 0;
	git_str path = GIT_STR_INIT;

	if (reflog_path(&path, repo, name) < 0)
		goto cleanup;

	ret = git_fs_path_isfile(git_str_cstr(&path));

cleanup:
	git_str_dispose(&path);
	return ret;
}

static int refdb_reflog_fs__has_log(git_refdb_backend *_backend, const char *name)
{
	refdb_fs_backend *backend;

	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(name);

	backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);

	return has_reflog(backend->repo, name);
}

static int refdb_reflog_fs__read(
	git_reflog **out,
	git_refdb_backend *_backend,
	const char *name)
{
	int error = -1;
	git_str log_path = GIT_STR_INIT;
	git_str log_file = GIT_STR_INIT;
	git_reflog *log = NULL;
	git_repository *repo;
	refdb_fs_backend *backend;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(name);

	backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	repo = backend->repo;

	if (reflog_alloc(&log, name, backend->oid_type) < 0)
		return -1;

	if (reflog_path(&log_path, repo, name) < 0)
		goto cleanup;

	error = git_futils_readbuffer(&log_file, git_str_cstr(&log_path));
	if (error < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if ((error == GIT_ENOTFOUND) &&
		((error = create_new_reflog_file(git_str_cstr(&log_path))) < 0))
		goto cleanup;

	if ((error = reflog_parse(log,
		git_str_cstr(&log_file), git_str_len(&log_file))) < 0)
		goto cleanup;

	*out = log;
	goto success;

cleanup:
	git_reflog_free(log);

success:
	git_str_dispose(&log_file);
	git_str_dispose(&log_path);

	return error;
}

static int serialize_reflog_entry(
	git_str *buf,
	const git_oid *oid_old,
	const git_oid *oid_new,
	const git_signature *committer,
	const char *msg)
{
	char raw_old[GIT_OID_MAX_HEXSIZE + 1];
	char raw_new[GIT_OID_MAX_HEXSIZE + 1];

	git_oid_tostr(raw_old, GIT_OID_MAX_HEXSIZE + 1, oid_old);
	git_oid_tostr(raw_new, GIT_OID_MAX_HEXSIZE + 1, oid_new);

	git_str_clear(buf);

	git_str_puts(buf, raw_old);
	git_str_putc(buf, ' ');
	git_str_puts(buf, raw_new);

	git_signature__writebuf(buf, " ", committer);

	/* drop trailing LF */
	git_str_rtrim(buf);

	if (msg) {
		size_t i;

		git_str_putc(buf, '\t');
		git_str_puts(buf, msg);

		for (i = 0; i < buf->size - 2; i++)
			if (buf->ptr[i] == '\n')
				buf->ptr[i] = ' ';
		git_str_rtrim(buf);
	}

	git_str_putc(buf, '\n');

	return git_str_oom(buf);
}

static int lock_reflog(git_filebuf *file, refdb_fs_backend *backend, const char *refname)
{
	git_repository *repo;
	git_str log_path = GIT_STR_INIT;
	int error;

	repo = backend->repo;

	if (!git_path_is_valid(backend->repo, refname, 0, GIT_FS_PATH_REJECT_FILESYSTEM_DEFAULTS)) {
		git_error_set(GIT_ERROR_INVALID, "invalid reference name '%s'", refname);
		return GIT_EINVALIDSPEC;
	}

	if (reflog_path(&log_path, repo, refname) < 0)
		return -1;

	if (!git_fs_path_isfile(git_str_cstr(&log_path))) {
		git_error_set(GIT_ERROR_INVALID,
			"log file for reference '%s' doesn't exist", refname);
		error = -1;
		goto cleanup;
	}

	error = git_filebuf_open(file, git_str_cstr(&log_path), 0, GIT_REFLOG_FILE_MODE);

cleanup:
	git_str_dispose(&log_path);

	return error;
}

static int refdb_reflog_fs__write(git_refdb_backend *_backend, git_reflog *reflog)
{
	int error = -1;
	unsigned int i;
	git_reflog_entry *entry;
	refdb_fs_backend *backend;
	git_str log = GIT_STR_INIT;
	git_filebuf fbuf = GIT_FILEBUF_INIT;

	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(reflog);

	backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);

	if ((error = lock_reflog(&fbuf, backend, reflog->ref_name)) < 0)
		return -1;

	git_vector_foreach(&reflog->entries, i, entry) {
		if (serialize_reflog_entry(&log, &(entry->oid_old), &(entry->oid_cur), entry->committer, entry->msg) < 0)
			goto cleanup;

		if ((error = git_filebuf_write(&fbuf, log.ptr, log.size)) < 0)
			goto cleanup;
	}

	error = git_filebuf_commit(&fbuf);
	goto success;

cleanup:
	git_filebuf_cleanup(&fbuf);

success:
	git_str_dispose(&log);

	return error;
}

/* Append to the reflog, must be called under reference lock */
static int reflog_append(
	refdb_fs_backend *backend,
	const git_reference *ref,
	const git_oid *old,
	const git_oid *new,
	const git_signature *who,
	const char *message)
{
	int error, is_symbolic, open_flags;
	git_oid old_id, new_id;
	git_str buf = GIT_STR_INIT, path = GIT_STR_INIT;
	git_repository *repo = backend->repo;

	is_symbolic = ref->type == GIT_REFERENCE_SYMBOLIC;

	/* "normal" symbolic updates do not write */
	if (is_symbolic &&
	    strcmp(ref->name, GIT_HEAD_FILE) &&
	    !(old && new))
		return 0;

	/* From here on is_symbolic also means that it's HEAD */

	git_oid_clear(&old_id, backend->oid_type);
	git_oid_clear(&new_id, backend->oid_type);

	if (old) {
		git_oid_cpy(&old_id, old);
	} else {
		error = git_reference_name_to_id(&old_id, repo, ref->name);
		if (error < 0 && error != GIT_ENOTFOUND)
			return error;
	}

	if (new) {
		git_oid_cpy(&new_id, new);
	} else {
		if (!is_symbolic) {
			git_oid_cpy(&new_id, git_reference_target(ref));
		} else {
			error = git_reference_name_to_id(&new_id, repo, git_reference_symbolic_target(ref));
			if (error < 0 && error != GIT_ENOTFOUND)
				return error;
			/* detaching HEAD does not create an entry */
			if (error == GIT_ENOTFOUND)
				return 0;

			git_error_clear();
		}
	}

	if ((error = serialize_reflog_entry(&buf, &old_id, &new_id, who, message)) < 0)
		goto cleanup;

	if ((error = reflog_path(&path, repo, ref->name)) < 0)
		goto cleanup;

	if (((error = git_futils_mkpath2file(git_str_cstr(&path), 0777)) < 0) &&
	    (error != GIT_EEXISTS)) {
		goto cleanup;
	}

	/* If the new branch matches part of the namespace of a previously deleted branch,
	 * there maybe an obsolete/unused directory (or directory hierarchy) in the way.
	 */
	if (git_fs_path_isdir(git_str_cstr(&path))) {
		if ((error = git_futils_rmdir_r(git_str_cstr(&path), NULL, GIT_RMDIR_SKIP_NONEMPTY)) < 0) {
			if (error == GIT_ENOTFOUND)
				error = 0;
		} else if (git_fs_path_isdir(git_str_cstr(&path))) {
			git_error_set(GIT_ERROR_REFERENCE, "cannot create reflog at '%s', there are reflogs beneath that folder",
				ref->name);
			error = GIT_EDIRECTORY;
		}

		if (error != 0)
			goto cleanup;
	}

	open_flags = O_WRONLY | O_CREAT | O_APPEND;

	if (backend->fsync)
		open_flags |= O_FSYNC;

	error = git_futils_writebuffer(&buf, git_str_cstr(&path), open_flags, GIT_REFLOG_FILE_MODE);

cleanup:
	git_str_dispose(&buf);
	git_str_dispose(&path);

	return error;
}

static int refdb_reflog_fs__rename(git_refdb_backend *_backend, const char *old_name, const char *new_name)
{
	int error = 0, fd;
	git_str old_path = GIT_STR_INIT;
	git_str new_path = GIT_STR_INIT;
	git_str temp_path = GIT_STR_INIT;
	git_str normalized = GIT_STR_INIT;
	git_repository *repo;
	refdb_fs_backend *backend;

	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(old_name);
	GIT_ASSERT_ARG(new_name);

	backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	repo = backend->repo;

	if ((error = git_reference__normalize_name(
		&normalized, new_name, GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL)) < 0)
			return error;

	if (git_str_joinpath(&temp_path, repo->gitdir, GIT_REFLOG_DIR) < 0)
		return -1;

	if ((error = loose_path(&old_path, git_str_cstr(&temp_path), old_name)) < 0)
		return error;

	if ((error = loose_path(&new_path, git_str_cstr(&temp_path), git_str_cstr(&normalized))) < 0)
		return error;

	if (!git_fs_path_exists(git_str_cstr(&old_path))) {
		error = GIT_ENOTFOUND;
		goto cleanup;
	}

	/*
	 * Move the reflog to a temporary place. This two-phase renaming is required
	 * in order to cope with funny renaming use cases when one tries to move a reference
	 * to a partially colliding namespace:
	 *  - a/b -> a/b/c
	 *  - a/b/c/d -> a/b/c
	 */
	if ((error = loose_path(&temp_path, git_str_cstr(&temp_path), "temp_reflog")) < 0)
		return error;

	if ((fd = git_futils_mktmp(&temp_path, git_str_cstr(&temp_path), GIT_REFLOG_FILE_MODE)) < 0) {
		error = -1;
		goto cleanup;
	}

	p_close(fd);

	if (p_rename(git_str_cstr(&old_path), git_str_cstr(&temp_path)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to rename reflog for %s", new_name);
		error = -1;
		goto cleanup;
	}

	if (git_fs_path_isdir(git_str_cstr(&new_path)) &&
		(git_futils_rmdir_r(git_str_cstr(&new_path), NULL, GIT_RMDIR_SKIP_NONEMPTY) < 0)) {
		error = -1;
		goto cleanup;
	}

	if (git_futils_mkpath2file(git_str_cstr(&new_path), GIT_REFLOG_DIR_MODE) < 0) {
		error = -1;
		goto cleanup;
	}

	if (p_rename(git_str_cstr(&temp_path), git_str_cstr(&new_path)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to rename reflog for %s", new_name);
		error = -1;
	}

cleanup:
	git_str_dispose(&temp_path);
	git_str_dispose(&old_path);
	git_str_dispose(&new_path);
	git_str_dispose(&normalized);

	return error;
}

static int refdb_reflog_fs__delete(git_refdb_backend *_backend, const char *name)
{
	refdb_fs_backend *backend = GIT_CONTAINER_OF(_backend, refdb_fs_backend, parent);
	git_str path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(name);

	if ((error = reflog_path(&path, backend->repo, name)) < 0)
		goto out;

	/*
	 * If a reference was moved downwards, eg refs/heads/br2 -> refs/heads/br2/new-name,
	 * refs/heads/br2 does exist but it's a directory. That's a valid situation.
	 * Proceed only if it's a file.
	 */
	if (!git_fs_path_isfile(path.ptr))
		goto out;

	if ((error = p_unlink(path.ptr)) < 0)
		goto out;

	error = refdb_fs_backend__prune_refs(backend, name, GIT_REFLOG_DIR);

out:
	git_str_dispose(&path);

	return error;
}

int git_refdb_backend_fs(
	git_refdb_backend **backend_out,
	git_repository *repository)
{
	int t = 0;
	git_str gitpath = GIT_STR_INIT;
	refdb_fs_backend *backend;

	backend = git__calloc(1, sizeof(refdb_fs_backend));
	GIT_ERROR_CHECK_ALLOC(backend);
	if (git_mutex_init(&backend->prlock) < 0) {
		git__free(backend);
		return -1;
	}


	if (git_refdb_init_backend(&backend->parent, GIT_REFDB_BACKEND_VERSION) < 0)
		goto fail;

	backend->repo = repository;
	backend->oid_type = repository->oid_type;

	if (repository->gitdir) {
		backend->gitpath = setup_namespace(repository, repository->gitdir);

		if (backend->gitpath == NULL)
			goto fail;
	}

	if (repository->commondir) {
		backend->commonpath = setup_namespace(repository, repository->commondir);

		if (backend->commonpath == NULL)
			goto fail;
	}

	if (git_str_joinpath(&gitpath, backend->commonpath, GIT_PACKEDREFS_FILE) < 0 ||
		git_sortedcache_new(
			&backend->refcache, offsetof(struct packref, name),
			NULL, NULL, packref_cmp, git_str_cstr(&gitpath)) < 0)
		goto fail;

	git_str_dispose(&gitpath);

	if (!git_repository__configmap_lookup(&t, backend->repo, GIT_CONFIGMAP_IGNORECASE) && t) {
		backend->iterator_flags |= GIT_ITERATOR_IGNORE_CASE;
		backend->direach_flags  |= GIT_FS_PATH_DIR_IGNORE_CASE;
	}
	if (!git_repository__configmap_lookup(&t, backend->repo, GIT_CONFIGMAP_PRECOMPOSE) && t) {
		backend->iterator_flags |= GIT_ITERATOR_PRECOMPOSE_UNICODE;
		backend->direach_flags  |= GIT_FS_PATH_DIR_PRECOMPOSE_UNICODE;
	}
	if ((!git_repository__configmap_lookup(&t, backend->repo, GIT_CONFIGMAP_FSYNCOBJECTFILES) && t) ||
		git_repository__fsync_gitdir)
		backend->fsync = 1;
	backend->iterator_flags |= GIT_ITERATOR_DESCEND_SYMLINKS;

	backend->parent.exists = &refdb_fs_backend__exists;
	backend->parent.lookup = &refdb_fs_backend__lookup;
	backend->parent.iterator = &refdb_fs_backend__iterator;
	backend->parent.write = &refdb_fs_backend__write;
	backend->parent.del = &refdb_fs_backend__delete;
	backend->parent.rename = &refdb_fs_backend__rename;
	backend->parent.compress = &refdb_fs_backend__compress;
	backend->parent.lock = &refdb_fs_backend__lock;
	backend->parent.unlock = &refdb_fs_backend__unlock;
	backend->parent.has_log = &refdb_reflog_fs__has_log;
	backend->parent.ensure_log = &refdb_reflog_fs__ensure_log;
	backend->parent.free = &refdb_fs_backend__free;
	backend->parent.reflog_read = &refdb_reflog_fs__read;
	backend->parent.reflog_write = &refdb_reflog_fs__write;
	backend->parent.reflog_rename = &refdb_reflog_fs__rename;
	backend->parent.reflog_delete = &refdb_reflog_fs__delete;

	*backend_out = (git_refdb_backend *)backend;
	return 0;

fail:
	git_mutex_free(&backend->prlock);
	git_str_dispose(&gitpath);
	git__free(backend->gitpath);
	git__free(backend->commonpath);
	git__free(backend);
	return -1;
}
