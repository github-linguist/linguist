/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "futils.h"

#include "runtime.h"
#include "hash.h"
#include "rand.h"
#include "hashmap_str.h"

#include <ctype.h>

#define GIT_FILEMODE_DEFAULT 0100666

int git_futils_mkpath2file(const char *file_path, const mode_t mode)
{
	return git_futils_mkdir(
		file_path, mode,
		GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST | GIT_MKDIR_VERIFY_DIR);
}

int git_futils_mktmp(git_str *path_out, const char *filename, mode_t mode)
{
	const int open_flags = O_RDWR | O_CREAT | O_EXCL | O_BINARY | O_CLOEXEC;
	unsigned int tries = 32;
	int fd;

	while (tries--) {
		uint64_t rand = git_rand_next();

		git_str_sets(path_out, filename);
		git_str_puts(path_out, "_git2_");
		git_str_encode_hexstr(path_out, (void *)&rand, sizeof(uint64_t));

		if (git_str_oom(path_out))
			return -1;

		/* Note that we open with O_CREAT | O_EXCL */
		if ((fd = p_open(path_out->ptr, open_flags, mode)) >= 0)
			return fd;
	}

	git_error_set(GIT_ERROR_OS,
		"failed to create temporary file '%s'", path_out->ptr);
	git_str_dispose(path_out);
	return -1;
}

int git_futils_creat_withpath(const char *path, const mode_t dirmode, const mode_t mode)
{
	int fd;

	if (git_futils_mkpath2file(path, dirmode) < 0)
		return -1;

	fd = p_creat(path, mode);
	if (fd < 0) {
		git_error_set(GIT_ERROR_OS, "failed to create file '%s'", path);
		return -1;
	}

	return fd;
}

int git_futils_creat_locked(const char *path, const mode_t mode)
{
	int fd = p_open(path, O_WRONLY | O_CREAT | O_EXCL | O_BINARY | O_CLOEXEC,
		mode);

	if (fd < 0) {
		int error = errno;
		git_error_set(GIT_ERROR_OS, "failed to create locked file '%s'", path);
		switch (error) {
		case EEXIST:
			return GIT_ELOCKED;
		case ENOENT:
			return GIT_ENOTFOUND;
		default:
			return -1;
		}
	}

	return fd;
}

int git_futils_creat_locked_withpath(const char *path, const mode_t dirmode, const mode_t mode)
{
	if (git_futils_mkpath2file(path, dirmode) < 0)
		return -1;

	return git_futils_creat_locked(path, mode);
}

int git_futils_open_ro(const char *path)
{
	int fd = p_open(path, O_RDONLY);
	if (fd < 0)
		return git_fs_path_set_error(errno, path, "open");
	return fd;
}

int git_futils_truncate(const char *path, int mode)
{
	int fd = p_open(path, O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, mode);
	if (fd < 0)
		return git_fs_path_set_error(errno, path, "open");

	close(fd);
	return 0;
}

int git_futils_filesize(uint64_t *out, git_file fd)
{
	struct stat sb;

	if (p_fstat(fd, &sb)) {
		git_error_set(GIT_ERROR_OS, "failed to stat file descriptor");
		return -1;
	}

	if (sb.st_size < 0) {
		git_error_set(GIT_ERROR_INVALID, "invalid file size");
		return -1;
	}

	*out = sb.st_size;
	return 0;
}

mode_t git_futils_canonical_mode(mode_t raw_mode)
{
	if (S_ISREG(raw_mode))
		return S_IFREG | GIT_PERMS_CANONICAL(raw_mode);
	else if (S_ISLNK(raw_mode))
		return S_IFLNK;
	else if (S_ISGITLINK(raw_mode))
		return S_IFGITLINK;
	else if (S_ISDIR(raw_mode))
		return S_IFDIR;
	else
		return 0;
}

int git_futils_readbuffer_fd(git_str *buf, git_file fd, size_t len)
{
	ssize_t read_size = 0;
	size_t alloc_len;

	git_str_clear(buf);

	if (!git__is_ssizet(len)) {
		git_error_set(GIT_ERROR_INVALID, "read too large");
		return -1;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, len, 1);
	if (git_str_grow(buf, alloc_len) < 0)
		return -1;

	/* p_read loops internally to read len bytes */
	read_size = p_read(fd, buf->ptr, len);

	if (read_size < 0) {
		git_error_set(GIT_ERROR_OS, "failed to read descriptor");
		git_str_dispose(buf);
		return -1;
	}

	if ((size_t)read_size != len) {
		git_error_set(GIT_ERROR_FILESYSTEM, "could not read (expected %" PRIuZ " bytes, read %" PRIuZ ")", len, (size_t)read_size);
		git_str_dispose(buf);
		return -1;
	}

	buf->ptr[read_size] = '\0';
	buf->size = read_size;

	return 0;
}

int git_futils_readbuffer_fd_full(git_str *buf, git_file fd)
{
	static size_t blocksize = 10240;
	size_t alloc_len = 0, total_size = 0;
	ssize_t read_size = 0;

	git_str_clear(buf);

	while (true) {
		GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, blocksize);

		if (git_str_grow(buf, alloc_len) < 0)
			return -1;

		/* p_read loops internally to read blocksize bytes */
		read_size = p_read(fd, buf->ptr, blocksize);

		if (read_size < 0) {
			git_error_set(GIT_ERROR_OS, "failed to read descriptor");
			git_str_dispose(buf);
			return -1;
		}

		total_size += read_size;

		if ((size_t)read_size < blocksize) {
			break;
		}
	}

	buf->ptr[total_size] = '\0';
	buf->size = total_size;

	return 0;
}

int git_futils_readbuffer_updated(
	git_str *out,
	const char *path,
	unsigned char checksum[GIT_HASH_SHA256_SIZE],
	int *updated)
{
	int error;
	git_file fd;
	struct stat st;
	git_str buf = GIT_STR_INIT;
	unsigned char checksum_new[GIT_HASH_SHA256_SIZE];

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(path && *path);

	if (updated != NULL)
		*updated = 0;

	if (p_stat(path, &st) < 0)
		return git_fs_path_set_error(errno, path, "stat");


	if (S_ISDIR(st.st_mode)) {
		git_error_set(GIT_ERROR_INVALID, "requested file is a directory");
		return GIT_ENOTFOUND;
	}

	if (!git__is_sizet(st.st_size+1)) {
		git_error_set(GIT_ERROR_OS, "invalid regular file stat for '%s'", path);
		return -1;
	}

	if ((fd = git_futils_open_ro(path)) < 0)
		return fd;

	if (git_futils_readbuffer_fd(&buf, fd, (size_t)st.st_size) < 0) {
		p_close(fd);
		return -1;
	}

	p_close(fd);

	if (checksum) {
		error = git_hash_buf(checksum_new, buf.ptr,
		                     buf.size, GIT_HASH_ALGORITHM_SHA256);

		if (error < 0) {
			git_str_dispose(&buf);
			return error;
		}

		/*
		 * If we were given a checksum, we only want to use it if it's different
		 */
		if (!memcmp(checksum, checksum_new, GIT_HASH_SHA256_SIZE)) {
			git_str_dispose(&buf);
			if (updated)
				*updated = 0;

			return 0;
		}

		memcpy(checksum, checksum_new, GIT_HASH_SHA256_SIZE);
	}

	/*
	 * If we're here, the file did change, or the user didn't have an old version
	 */
	if (updated != NULL)
		*updated = 1;

	git_str_swap(out, &buf);
	git_str_dispose(&buf);

	return 0;
}

int git_futils_readbuffer(git_str *buf, const char *path)
{
	return git_futils_readbuffer_updated(buf, path, NULL, NULL);
}

int git_futils_writebuffer(
	const git_str *buf, const char *path, int flags, mode_t mode)
{
	int fd, do_fsync = 0, error = 0;

	if (!flags)
		flags = O_CREAT | O_TRUNC | O_WRONLY;

	if ((flags & O_FSYNC) != 0)
		do_fsync = 1;

	flags &= ~O_FSYNC;

	if (!mode)
		mode = GIT_FILEMODE_DEFAULT;

	if ((fd = p_open(path, flags, mode)) < 0) {
		git_error_set(GIT_ERROR_OS, "could not open '%s' for writing", path);
		return fd;
	}

	if ((error = p_write(fd, git_str_cstr(buf), git_str_len(buf))) < 0) {
		git_error_set(GIT_ERROR_OS, "could not write to '%s'", path);
		(void)p_close(fd);
		return error;
	}

	if (do_fsync && (error = p_fsync(fd)) < 0) {
		git_error_set(GIT_ERROR_OS, "could not fsync '%s'", path);
		p_close(fd);
		return error;
	}

	if ((error = p_close(fd)) < 0) {
		git_error_set(GIT_ERROR_OS, "error while closing '%s'", path);
		return error;
	}

	if (do_fsync && (flags & O_CREAT))
		error = git_futils_fsync_parent(path);

	return error;
}

int git_futils_mv_withpath(const char *from, const char *to, const mode_t dirmode)
{
	if (git_futils_mkpath2file(to, dirmode) < 0)
		return -1;

	if (p_rename(from, to) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to rename '%s' to '%s'", from, to);
		return -1;
	}

	return 0;
}

int git_futils_mmap_ro(git_map *out, git_file fd, off64_t begin, size_t len)
{
	return p_mmap(out, len, GIT_PROT_READ, GIT_MAP_SHARED, fd, begin);
}

int git_futils_mmap_ro_file(git_map *out, const char *path)
{
	git_file fd = git_futils_open_ro(path);
	uint64_t len;
	int result;

	if (fd < 0)
		return fd;

	if ((result = git_futils_filesize(&len, fd)) < 0)
		goto out;

	if (!git__is_sizet(len)) {
		git_error_set(GIT_ERROR_OS, "file `%s` too large to mmap", path);
		result = -1;
		goto out;
	}

	result = git_futils_mmap_ro(out, fd, 0, (size_t)len);
out:
	p_close(fd);
	return result;
}

void git_futils_mmap_free(git_map *out)
{
	p_munmap(out);
}

GIT_INLINE(int) mkdir_validate_dir(
	const char *path,
	struct stat *st,
	mode_t mode,
	uint32_t flags,
	struct git_futils_mkdir_options *opts)
{
	/* with exclusive create, existing dir is an error */
	if ((flags & GIT_MKDIR_EXCL) != 0) {
		git_error_set(GIT_ERROR_FILESYSTEM,
			"failed to make directory '%s': directory exists", path);
		return GIT_EEXISTS;
	}

	if ((S_ISREG(st->st_mode) && (flags & GIT_MKDIR_REMOVE_FILES)) ||
		(S_ISLNK(st->st_mode) && (flags & GIT_MKDIR_REMOVE_SYMLINKS))) {
		if (p_unlink(path) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to remove %s '%s'",
				S_ISLNK(st->st_mode) ? "symlink" : "file", path);
			return GIT_EEXISTS;
		}

		opts->perfdata.mkdir_calls++;

		if (p_mkdir(path, mode) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to make directory '%s'", path);
			return GIT_EEXISTS;
		}
	}

	else if (S_ISLNK(st->st_mode)) {
		/* Re-stat the target, make sure it's a directory */
		opts->perfdata.stat_calls++;

		if (p_stat(path, st) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to make directory '%s'", path);
			return GIT_EEXISTS;
		}
	}

	else if (!S_ISDIR(st->st_mode)) {
		git_error_set(GIT_ERROR_FILESYSTEM,
			"failed to make directory '%s': directory exists", path);
		return GIT_EEXISTS;
	}

	return 0;
}

GIT_INLINE(int) mkdir_validate_mode(
	const char *path,
	struct stat *st,
	bool terminal_path,
	mode_t mode,
	uint32_t flags,
	struct git_futils_mkdir_options *opts)
{
	if (((terminal_path && (flags & GIT_MKDIR_CHMOD) != 0) ||
		(flags & GIT_MKDIR_CHMOD_PATH) != 0) && st->st_mode != mode) {

		opts->perfdata.chmod_calls++;

		if (p_chmod(path, mode) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to set permissions on '%s'", path);
			return -1;
		}
	}

	return 0;
}

GIT_INLINE(int) mkdir_canonicalize(
	git_str *path,
	uint32_t flags)
{
	ssize_t root_len;

	if (path->size == 0) {
		git_error_set(GIT_ERROR_OS, "attempt to create empty path");
		return -1;
	}

	/* Trim trailing slashes (except the root) */
	if ((root_len = git_fs_path_root(path->ptr)) < 0)
		root_len = 0;
	else
		root_len++;

	while (path->size > (size_t)root_len && path->ptr[path->size - 1] == '/')
		path->ptr[--path->size] = '\0';

	/* if we are not supposed to made the last element, truncate it */
	if ((flags & GIT_MKDIR_SKIP_LAST2) != 0) {
		git_fs_path_dirname_r(path, path->ptr);
		flags |= GIT_MKDIR_SKIP_LAST;
	}
	if ((flags & GIT_MKDIR_SKIP_LAST) != 0) {
		git_fs_path_dirname_r(path, path->ptr);
	}

	/* We were either given the root path (or trimmed it to
	* the root), we don't have anything to do.
	*/
	if (path->size <= (size_t)root_len)
		git_str_clear(path);

	return 0;
}

int git_futils_mkdir(
	const char *path,
	mode_t mode,
	uint32_t flags)
{
	git_str make_path = GIT_STR_INIT, parent_path = GIT_STR_INIT;
	const char *relative;
	struct git_futils_mkdir_options opts = { 0 };
	struct stat st;
	size_t depth = 0;
	int len = 0, root_len, error;

	if ((error = git_str_puts(&make_path, path)) < 0 ||
		(error = mkdir_canonicalize(&make_path, flags)) < 0 ||
		(error = git_str_puts(&parent_path, make_path.ptr)) < 0 ||
		make_path.size == 0)
		goto done;

	root_len = git_fs_path_root(make_path.ptr);

	/* find the first parent directory that exists.  this will be used
	 * as the base to dirname_relative.
	 */
	for (relative = make_path.ptr; parent_path.size; ) {
		error = p_lstat(parent_path.ptr, &st);

		if (error == 0) {
			break;
		} else if (errno != ENOENT) {
			git_error_set(GIT_ERROR_OS, "failed to stat '%s'", parent_path.ptr);
			error = -1;
			goto done;
		}

		depth++;

		/* examine the parent of the current path */
		if ((len = git_fs_path_dirname_r(&parent_path, parent_path.ptr)) < 0) {
			error = len;
			goto done;
		}

		GIT_ASSERT(len);

		/*
		 * We've walked all the given path's parents and it's either relative
		 * (the parent is simply '.') or rooted (the length is less than or
		 * equal to length of the root path).  The path may be less than the
		 * root path length on Windows, where `C:` == `C:/`.
		 */
		if ((len == 1 && parent_path.ptr[0] == '.') ||
		    (len == 1 && parent_path.ptr[0] == '/') ||
		    len <= root_len) {
			relative = make_path.ptr;
			break;
		}

		relative = make_path.ptr + len + 1;

		/* not recursive? just make this directory relative to its parent. */
		if ((flags & GIT_MKDIR_PATH) == 0)
			break;
	}

	/* we found an item at the location we're trying to create,
	 * validate it.
	 */
	if (depth == 0) {
		error = mkdir_validate_dir(make_path.ptr, &st, mode, flags, &opts);

		if (!error)
			error = mkdir_validate_mode(
				make_path.ptr, &st, true, mode, flags, &opts);

		goto done;
	}

	/* we already took `SKIP_LAST` and `SKIP_LAST2` into account when
	 * canonicalizing `make_path`.
	 */
	flags &= ~(GIT_MKDIR_SKIP_LAST2 | GIT_MKDIR_SKIP_LAST);

	error = git_futils_mkdir_relative(relative,
		parent_path.size ? parent_path.ptr : NULL, mode, flags, &opts);

done:
	git_str_dispose(&make_path);
	git_str_dispose(&parent_path);
	return error;
}

int git_futils_mkdir_r(const char *path, const mode_t mode)
{
	return git_futils_mkdir(path, mode, GIT_MKDIR_PATH);
}

int git_futils_mkdir_relative(
	const char *relative_path,
	const char *base,
	mode_t mode,
	uint32_t flags,
	struct git_futils_mkdir_options *opts)
{
	git_str make_path = GIT_STR_INIT;
	ssize_t root = 0, min_root_len;
	char lastch = '/', *tail;
	struct stat st;
	struct git_futils_mkdir_options empty_opts = {0};
	int error;

	if (!opts)
		opts = &empty_opts;

	/* build path and find "root" where we should start calling mkdir */
	if (git_fs_path_join_unrooted(&make_path, relative_path, base, &root) < 0)
		return -1;

	if ((error = mkdir_canonicalize(&make_path, flags)) < 0 ||
		make_path.size == 0)
		goto done;

	/* if we are not supposed to make the whole path, reset root */
	if ((flags & GIT_MKDIR_PATH) == 0)
		root = git_str_rfind(&make_path, '/');

	/* advance root past drive name or network mount prefix */
	min_root_len = git_fs_path_root(make_path.ptr);
	if (root < min_root_len)
		root = min_root_len;
	while (root >= 0 && make_path.ptr[root] == '/')
		++root;

	/* clip root to make_path length */
	if (root > (ssize_t)make_path.size)
		root = (ssize_t)make_path.size; /* i.e. NUL byte of string */
	if (root < 0)
		root = 0;

	/* walk down tail of path making each directory */
	for (tail = &make_path.ptr[root]; *tail; *tail = lastch) {
		bool mkdir_attempted = false;

		/* advance tail to include next path component */
		while (*tail == '/')
			tail++;
		while (*tail && *tail != '/')
			tail++;

		/* truncate path at next component */
		lastch = *tail;
		*tail = '\0';
		st.st_mode = 0;

		if (opts->cache_pathset &&
		    git_hashset_str_contains(opts->cache_pathset, make_path.ptr))
			continue;

		/* See what's going on with this path component */
		opts->perfdata.stat_calls++;

retry_lstat:
		if (p_lstat(make_path.ptr, &st) < 0) {
			if (mkdir_attempted || errno != ENOENT) {
				git_error_set(GIT_ERROR_OS, "cannot access component in path '%s'", make_path.ptr);
				error = -1;
				goto done;
			}

			git_error_clear();
			opts->perfdata.mkdir_calls++;
			mkdir_attempted = true;
			if (p_mkdir(make_path.ptr, mode) < 0) {
				if (errno == EEXIST)
					goto retry_lstat;
				git_error_set(GIT_ERROR_OS, "failed to make directory '%s'", make_path.ptr);
				error = -1;
				goto done;
			}
		} else {
			if ((error = mkdir_validate_dir(
				make_path.ptr, &st, mode, flags, opts)) < 0)
				goto done;
		}

		/* chmod if requested and necessary */
		if ((error = mkdir_validate_mode(
			make_path.ptr, &st, (lastch == '\0'), mode, flags, opts)) < 0)
			goto done;

		if (opts->cache_pathset && opts->cache_pool) {
			char *cache_path;
			size_t alloc_size;

			GIT_ERROR_CHECK_ALLOC_ADD(&alloc_size, make_path.size, 1);
			cache_path = git_pool_malloc(opts->cache_pool, alloc_size);
			GIT_ERROR_CHECK_ALLOC(cache_path);

			memcpy(cache_path, make_path.ptr, make_path.size + 1);

			if ((error = git_hashset_str_add(opts->cache_pathset, cache_path)) < 0)
				goto done;
		}
	}

	error = 0;

	/* check that full path really is a directory if requested & needed */
	if ((flags & GIT_MKDIR_VERIFY_DIR) != 0 &&
		lastch != '\0') {
		opts->perfdata.stat_calls++;

		if (p_stat(make_path.ptr, &st) < 0 || !S_ISDIR(st.st_mode)) {
			git_error_set(GIT_ERROR_OS, "path is not a directory '%s'",
				make_path.ptr);
			error = GIT_ENOTFOUND;
		}
	}

done:
	git_str_dispose(&make_path);
	return error;
}

typedef struct {
	const char *base;
	size_t baselen;
	uint32_t flags;
	int depth;
} futils__rmdir_data;

#define FUTILS_MAX_DEPTH 100

static int futils__error_cannot_rmdir(const char *path, const char *filemsg)
{
	if (filemsg)
		git_error_set(GIT_ERROR_OS, "could not remove directory '%s': %s",
				   path, filemsg);
	else
		git_error_set(GIT_ERROR_OS, "could not remove directory '%s'", path);

	return -1;
}

static int futils__rm_first_parent(git_str *path, const char *ceiling)
{
	int error = GIT_ENOTFOUND;
	struct stat st;

	while (error == GIT_ENOTFOUND) {
		git_str_rtruncate_at_char(path, '/');

		if (!path->size || git__prefixcmp(path->ptr, ceiling) != 0)
			error = 0;
		else if (p_lstat_posixly(path->ptr, &st) == 0) {
			if (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode))
				error = p_unlink(path->ptr);
			else if (!S_ISDIR(st.st_mode))
				error = -1; /* fail to remove non-regular file */
		} else if (errno != ENOTDIR)
			error = -1;
	}

	if (error)
		futils__error_cannot_rmdir(path->ptr, "cannot remove parent");

	return error;
}

static int futils__rmdir_recurs_foreach(void *opaque, git_str *path)
{
	int error = 0;
	futils__rmdir_data *data = opaque;
	struct stat st;

	if (data->depth > FUTILS_MAX_DEPTH)
		error = futils__error_cannot_rmdir(
			path->ptr, "directory nesting too deep");

	else if ((error = p_lstat_posixly(path->ptr, &st)) < 0) {
		if (errno == ENOENT)
			error = 0;
		else if (errno == ENOTDIR) {
			/* asked to remove a/b/c/d/e and a/b is a normal file */
			if ((data->flags & GIT_RMDIR_REMOVE_BLOCKERS) != 0)
				error = futils__rm_first_parent(path, data->base);
			else
				futils__error_cannot_rmdir(
					path->ptr, "parent is not directory");
		}
		else
			error = git_fs_path_set_error(errno, path->ptr, "rmdir");
	}

	else if (S_ISDIR(st.st_mode)) {
		data->depth++;

		error = git_fs_path_direach(path, 0, futils__rmdir_recurs_foreach, data);

		data->depth--;

		if (error < 0)
			return error;

		if (data->depth == 0 && (data->flags & GIT_RMDIR_SKIP_ROOT) != 0)
			return error;

		if ((error = p_rmdir(path->ptr)) < 0) {
			if ((data->flags & GIT_RMDIR_SKIP_NONEMPTY) != 0 &&
				(errno == ENOTEMPTY || errno == EEXIST || errno == EBUSY))
				error = 0;
			else
				error = git_fs_path_set_error(errno, path->ptr, "rmdir");
		}
	}

	else if ((data->flags & GIT_RMDIR_REMOVE_FILES) != 0) {
		if (p_unlink(path->ptr) < 0)
			error = git_fs_path_set_error(errno, path->ptr, "remove");
	}

	else if ((data->flags & GIT_RMDIR_SKIP_NONEMPTY) == 0)
		error = futils__error_cannot_rmdir(path->ptr, "still present");

	return error;
}

static int futils__rmdir_empty_parent(void *opaque, const char *path)
{
	futils__rmdir_data *data = opaque;
	int error = 0;

	if (strlen(path) <= data->baselen)
		error = GIT_ITEROVER;

	else if (p_rmdir(path) < 0) {
		int en = errno;

		if (en == ENOENT || en == ENOTDIR) {
			/* do nothing */
		} else if ((data->flags & GIT_RMDIR_SKIP_NONEMPTY) == 0 &&
			en == EBUSY) {
			error = git_fs_path_set_error(errno, path, "rmdir");
		} else if (en == ENOTEMPTY || en == EEXIST || en == EBUSY) {
			error = GIT_ITEROVER;
		} else {
			error = git_fs_path_set_error(errno, path, "rmdir");
		}
	}

	return error;
}

int git_futils_rmdir_r(
	const char *path, const char *base, uint32_t flags)
{
	int error;
	git_str fullpath = GIT_STR_INIT;
	futils__rmdir_data data;

	/* build path and find "root" where we should start calling mkdir */
	if (git_fs_path_join_unrooted(&fullpath, path, base, NULL) < 0)
		return -1;

	memset(&data, 0, sizeof(data));
	data.base    = base ? base : "";
	data.baselen = base ? strlen(base) : 0;
	data.flags   = flags;

	error = futils__rmdir_recurs_foreach(&data, &fullpath);

	/* remove now-empty parents if requested */
	if (!error && (flags & GIT_RMDIR_EMPTY_PARENTS) != 0)
		error = git_fs_path_walk_up(
			&fullpath, base, futils__rmdir_empty_parent, &data);

	if (error == GIT_ITEROVER) {
		git_error_clear();
		error = 0;
	}

	git_str_dispose(&fullpath);

	return error;
}

int git_futils_fake_symlink(const char *target, const char *path)
{
	int retcode = GIT_ERROR;
	int fd = git_futils_creat_withpath(path, 0755, 0644);
	if (fd >= 0) {
		retcode = p_write(fd, target, strlen(target));
		p_close(fd);
	}
	return retcode;
}

static int cp_by_fd(int ifd, int ofd, bool close_fd_when_done)
{
	int error = 0;
	char buffer[GIT_BUFSIZE_FILEIO];
	ssize_t len = 0;

	while (!error && (len = p_read(ifd, buffer, sizeof(buffer))) > 0)
		/* p_write() does not have the same semantics as write().  It loops
		 * internally and will return 0 when it has completed writing.
		 */
		error = p_write(ofd, buffer, len);

	if (len < 0) {
		git_error_set(GIT_ERROR_OS, "read error while copying file");
		error = (int)len;
	}

	if (error < 0)
		git_error_set(GIT_ERROR_OS, "write error while copying file");

	if (close_fd_when_done) {
		p_close(ifd);
		p_close(ofd);
	}

	return error;
}

int git_futils_cp(const char *from, const char *to, mode_t filemode)
{
	int ifd, ofd;

	if ((ifd = git_futils_open_ro(from)) < 0)
		return ifd;

	if ((ofd = p_open(to, O_WRONLY | O_CREAT | O_EXCL, filemode)) < 0) {
		p_close(ifd);
		return git_fs_path_set_error(errno, to, "open for writing");
	}

	return cp_by_fd(ifd, ofd, true);
}

int git_futils_touch(const char *path, time_t *when)
{
	struct p_timeval times[2];
	int ret;

	times[0].tv_sec =  times[1].tv_sec  = when ? *when : time(NULL);
	times[0].tv_usec = times[1].tv_usec = 0;

	ret = p_utimes(path, times);

	return (ret < 0) ? git_fs_path_set_error(errno, path, "touch") : 0;
}

static int cp_link(const char *from, const char *to, size_t link_size)
{
	int error = 0;
	ssize_t read_len;
	char *link_data;
	size_t alloc_size;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_size, link_size, 1);
	link_data = git__malloc(alloc_size);
	GIT_ERROR_CHECK_ALLOC(link_data);

	read_len = p_readlink(from, link_data, link_size);
	if (read_len != (ssize_t)link_size) {
		git_error_set(GIT_ERROR_OS, "failed to read symlink data for '%s'", from);
		error = -1;
	}
	else {
		link_data[read_len] = '\0';

		if (p_symlink(link_data, to) < 0) {
			git_error_set(GIT_ERROR_OS, "could not symlink '%s' as '%s'",
				link_data, to);
			error = -1;
		}
	}

	git__free(link_data);
	return error;
}

typedef struct {
	const char *to_root;
	git_str to;
	ssize_t from_prefix;
	uint32_t flags;
	uint32_t mkdir_flags;
	mode_t dirmode;
} cp_r_info;

#define GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT (1u << 10)

static int _cp_r_mkdir(cp_r_info *info, git_str *from)
{
	int error = 0;

	/* create root directory the first time we need to create a directory */
	if ((info->flags & GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT) == 0) {
		error = git_futils_mkdir(
			info->to_root, info->dirmode,
			(info->flags & GIT_CPDIR_CHMOD_DIRS) ? GIT_MKDIR_CHMOD : 0);

		info->flags |= GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT;
	}

	/* create directory with root as base to prevent excess chmods */
	if (!error)
		error = git_futils_mkdir_relative(
			from->ptr + info->from_prefix, info->to_root,
			info->dirmode, info->mkdir_flags, NULL);

	return error;
}

static int _cp_r_callback(void *ref, git_str *from)
{
	int error = 0;
	cp_r_info *info = ref;
	struct stat from_st, to_st;
	bool exists = false;

	if ((info->flags & GIT_CPDIR_COPY_DOTFILES) == 0 &&
		from->ptr[git_fs_path_basename_offset(from)] == '.')
		return 0;

	if ((error = git_str_joinpath(
			&info->to, info->to_root, from->ptr + info->from_prefix)) < 0)
		return error;

	if (!(error = git_fs_path_lstat(info->to.ptr, &to_st)))
		exists = true;
	else if (error != GIT_ENOTFOUND)
		return error;
	else {
		git_error_clear();
		error = 0;
	}

	if ((error = git_fs_path_lstat(from->ptr, &from_st)) < 0)
		return error;

	if (S_ISDIR(from_st.st_mode)) {
		mode_t oldmode = info->dirmode;

		/* if we are not chmod'ing, then overwrite dirmode */
		if ((info->flags & GIT_CPDIR_CHMOD_DIRS) == 0)
			info->dirmode = from_st.st_mode;

		/* make directory now if CREATE_EMPTY_DIRS is requested and needed */
		if (!exists && (info->flags & GIT_CPDIR_CREATE_EMPTY_DIRS) != 0)
			error = _cp_r_mkdir(info, from);

		/* recurse onto target directory */
		if (!error && (!exists || S_ISDIR(to_st.st_mode)))
			error = git_fs_path_direach(from, 0, _cp_r_callback, info);

		if (oldmode != 0)
			info->dirmode = oldmode;

		return error;
	}

	if (exists) {
		if ((info->flags & GIT_CPDIR_OVERWRITE) == 0)
			return 0;

		if (p_unlink(info->to.ptr) < 0) {
			git_error_set(GIT_ERROR_OS, "cannot overwrite existing file '%s'",
				info->to.ptr);
			return GIT_EEXISTS;
		}
	}

	/* Done if this isn't a regular file or a symlink */
	if (!S_ISREG(from_st.st_mode) &&
		(!S_ISLNK(from_st.st_mode) ||
		 (info->flags & GIT_CPDIR_COPY_SYMLINKS) == 0))
		return 0;

	/* Make container directory on demand if needed */
	if ((info->flags & GIT_CPDIR_CREATE_EMPTY_DIRS) == 0 &&
		(error = _cp_r_mkdir(info, from)) < 0)
		return error;

	/* make symlink or regular file */
	if (info->flags & GIT_CPDIR_LINK_FILES) {
		if ((error = p_link(from->ptr, info->to.ptr)) < 0)
			git_error_set(GIT_ERROR_OS, "failed to link '%s'", from->ptr);
	} else if (S_ISLNK(from_st.st_mode)) {
		error = cp_link(from->ptr, info->to.ptr, (size_t)from_st.st_size);
	} else {
		mode_t usemode = from_st.st_mode;

		if ((info->flags & GIT_CPDIR_SIMPLE_TO_MODE) != 0)
			usemode = GIT_PERMS_FOR_WRITE(usemode);

		error = git_futils_cp(from->ptr, info->to.ptr, usemode);
	}

	return error;
}

int git_futils_cp_r(
	const char *from,
	const char *to,
	uint32_t flags,
	mode_t dirmode)
{
	int error;
	git_str path = GIT_STR_INIT;
	cp_r_info info;

	if (git_str_joinpath(&path, from, "") < 0) /* ensure trailing slash */
		return -1;

	memset(&info, 0, sizeof(info));
	info.to_root = to;
	info.flags   = flags;
	info.dirmode = dirmode;
	info.from_prefix = path.size;
	git_str_init(&info.to, 0);

	/* precalculate mkdir flags */
	if ((flags & GIT_CPDIR_CREATE_EMPTY_DIRS) == 0) {
		/* if not creating empty dirs, then use mkdir to create the path on
		 * demand right before files are copied.
		 */
		info.mkdir_flags = GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST;
		if ((flags & GIT_CPDIR_CHMOD_DIRS) != 0)
			info.mkdir_flags |= GIT_MKDIR_CHMOD_PATH;
	} else {
		/* otherwise, we will do simple mkdir as directories are encountered */
		info.mkdir_flags =
			((flags & GIT_CPDIR_CHMOD_DIRS) != 0) ? GIT_MKDIR_CHMOD : 0;
	}

	error = _cp_r_callback(&info, &path);

	git_str_dispose(&path);
	git_str_dispose(&info.to);

	return error;
}

int git_futils_filestamp_check(
	git_futils_filestamp *stamp, const char *path)
{
	struct stat st;

	/* if the stamp is NULL, then always reload */
	if (stamp == NULL)
		return 1;

	if (p_stat(path, &st) < 0)
		return GIT_ENOTFOUND;

	if (stamp->mtime.tv_sec == st.st_mtime &&
#if defined(GIT_USE_NSEC)
		stamp->mtime.tv_nsec == st.st_mtime_nsec &&
#endif
		stamp->size  == (uint64_t)st.st_size   &&
		stamp->ino   == (unsigned int)st.st_ino)
		return 0;

	stamp->mtime.tv_sec = st.st_mtime;
#if defined(GIT_USE_NSEC)
	stamp->mtime.tv_nsec = st.st_mtime_nsec;
#endif
	stamp->size  = (uint64_t)st.st_size;
	stamp->ino   = (unsigned int)st.st_ino;

	return 1;
}

void git_futils_filestamp_set(
	git_futils_filestamp *target, const git_futils_filestamp *source)
{
	if (source)
		memcpy(target, source, sizeof(*target));
	else
		memset(target, 0, sizeof(*target));
}


void git_futils_filestamp_set_from_stat(
	git_futils_filestamp *stamp, struct stat *st)
{
	if (st) {
		stamp->mtime.tv_sec = st->st_mtime;
#if defined(GIT_USE_NSEC)
		stamp->mtime.tv_nsec = st->st_mtime_nsec;
#else
		stamp->mtime.tv_nsec = 0;
#endif
		stamp->size  = (uint64_t)st->st_size;
		stamp->ino   = (unsigned int)st->st_ino;
	} else {
		memset(stamp, 0, sizeof(*stamp));
	}
}

int git_futils_fsync_dir(const char *path)
{
#ifdef GIT_WIN32
	GIT_UNUSED(path);
	return 0;
#else
	int fd, error = -1;

	if ((fd = p_open(path, O_RDONLY)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to open directory '%s' for fsync", path);
		return -1;
	}

	if ((error = p_fsync(fd)) < 0)
		git_error_set(GIT_ERROR_OS, "failed to fsync directory '%s'", path);

	p_close(fd);
	return error;
#endif
}

int git_futils_fsync_parent(const char *path)
{
	char *parent;
	int error;

	if ((parent = git_fs_path_dirname(path)) == NULL)
		return -1;

	error = git_futils_fsync_dir(parent);
	git__free(parent);
	return error;
}
