/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "filebuf.h"

#include "futils.h"

static const size_t WRITE_BUFFER_SIZE = (4096 * 2);

enum buferr_t {
	BUFERR_OK = 0,
	BUFERR_WRITE,
	BUFERR_ZLIB,
	BUFERR_MEM
};

#define ENSURE_BUF_OK(buf) if ((buf)->last_error != BUFERR_OK) { return -1; }

static int verify_last_error(git_filebuf *file)
{
	switch (file->last_error) {
	case BUFERR_WRITE:
		git_error_set(GIT_ERROR_OS, "failed to write out file");
		return -1;

	case BUFERR_MEM:
		git_error_set_oom();
		return -1;

	case BUFERR_ZLIB:
		git_error_set(GIT_ERROR_ZLIB,
			"Buffer error when writing out ZLib data");
		return -1;

	default:
		return 0;
	}
}

static int lock_file(git_filebuf *file, int flags, mode_t mode)
{
	if (git_fs_path_exists(file->path_lock) == true) {
		git_error_clear(); /* actual OS error code just confuses */
		git_error_set(GIT_ERROR_OS,
			"failed to lock file '%s' for writing", file->path_lock);
		return GIT_ELOCKED;
	}

	/* create path to the file buffer is required */
	if (flags & GIT_FILEBUF_CREATE_LEADING_DIRS) {
		/* XXX: Should dirmode here be configurable? Or is 0777 always fine? */
		file->fd = git_futils_creat_locked_withpath(file->path_lock, 0777, mode);
	} else {
		file->fd = git_futils_creat_locked(file->path_lock, mode);
	}

	if (file->fd < 0)
		return file->fd;

	file->fd_is_open = true;

	if ((flags & GIT_FILEBUF_APPEND) && git_fs_path_exists(file->path_original) == true) {
		git_file source;
		char buffer[GIT_BUFSIZE_FILEIO];
		ssize_t read_bytes;
		int error = 0;

		source = p_open(file->path_original, O_RDONLY);
		if (source < 0) {
			git_error_set(GIT_ERROR_OS,
				"failed to open file '%s' for reading",
				file->path_original);
			return -1;
		}

		while ((read_bytes = p_read(source, buffer, sizeof(buffer))) > 0) {
			if ((error = p_write(file->fd, buffer, read_bytes)) < 0)
				break;
			if (file->compute_digest)
				git_hash_update(&file->digest, buffer, read_bytes);
		}

		p_close(source);

		if (read_bytes < 0) {
			git_error_set(GIT_ERROR_OS, "failed to read file '%s'", file->path_original);
			return -1;
		} else if (error < 0) {
			git_error_set(GIT_ERROR_OS, "failed to write file '%s'", file->path_lock);
			return -1;
		}
	}

	return 0;
}

void git_filebuf_cleanup(git_filebuf *file)
{
	if (file->fd_is_open && file->fd >= 0)
		p_close(file->fd);

	if (file->created_lock && !file->did_rename && file->path_lock && git_fs_path_exists(file->path_lock))
		p_unlink(file->path_lock);

	if (file->compute_digest) {
		git_hash_ctx_cleanup(&file->digest);
		file->compute_digest = 0;
	}

	if (file->buffer)
		git__free(file->buffer);

	/* use the presence of z_buf to decide if we need to deflateEnd */
	if (file->z_buf) {
		git__free(file->z_buf);
		deflateEnd(&file->zs);
	}

	if (file->path_original)
		git__free(file->path_original);
	if (file->path_lock)
		git__free(file->path_lock);

	memset(file, 0x0, sizeof(git_filebuf));
	file->fd = -1;
}

GIT_INLINE(int) flush_buffer(git_filebuf *file)
{
	int result = file->write(file, file->buffer, file->buf_pos);
	file->buf_pos = 0;
	return result;
}

int git_filebuf_flush(git_filebuf *file)
{
	return flush_buffer(file);
}

static int write_normal(git_filebuf *file, void *source, size_t len)
{
	if (len > 0) {
		if (p_write(file->fd, (void *)source, len) < 0) {
			file->last_error = BUFERR_WRITE;
			return -1;
		}

		if (file->compute_digest)
			git_hash_update(&file->digest, source, len);
	}

	return 0;
}

static int write_deflate(git_filebuf *file, void *source, size_t len)
{
	z_stream *zs = &file->zs;

	if (len > 0 || file->flush_mode == Z_FINISH) {
		zs->next_in = source;
		zs->avail_in = (uInt)len;

		do {
			size_t have;

			zs->next_out = file->z_buf;
			zs->avail_out = (uInt)file->buf_size;

			if (deflate(zs, file->flush_mode) == Z_STREAM_ERROR) {
				file->last_error = BUFERR_ZLIB;
				return -1;
			}

			have = file->buf_size - (size_t)zs->avail_out;

			if (p_write(file->fd, file->z_buf, have) < 0) {
				file->last_error = BUFERR_WRITE;
				return -1;
			}

		} while (zs->avail_out == 0);

		GIT_ASSERT(zs->avail_in == 0);

		if (file->compute_digest)
			git_hash_update(&file->digest, source, len);
	}

	return 0;
}

#define MAX_SYMLINK_DEPTH 5

static int resolve_symlink(git_str *out, const char *path)
{
	int i, error, root;
	ssize_t ret;
	struct stat st;
	git_str curpath = GIT_STR_INIT, target = GIT_STR_INIT;

	if ((error = git_str_grow(&target, GIT_PATH_MAX + 1)) < 0 ||
	    (error = git_str_puts(&curpath, path)) < 0)
		return error;

	for (i = 0; i < MAX_SYMLINK_DEPTH; i++) {
		error = p_lstat(curpath.ptr, &st);
		if (error < 0 && errno == ENOENT) {
			error = git_str_puts(out, curpath.ptr);
			goto cleanup;
		}

		if (error < 0) {
			git_error_set(GIT_ERROR_OS, "failed to stat '%s'", curpath.ptr);
			error = -1;
			goto cleanup;
		}

		if (!S_ISLNK(st.st_mode)) {
			error = git_str_puts(out, curpath.ptr);
			goto cleanup;
		}

		ret = p_readlink(curpath.ptr, target.ptr, GIT_PATH_MAX);
		if (ret < 0) {
			git_error_set(GIT_ERROR_OS, "failed to read symlink '%s'", curpath.ptr);
			error = -1;
			goto cleanup;
		}

		if (ret == GIT_PATH_MAX) {
			git_error_set(GIT_ERROR_INVALID, "symlink target too long");
			error = -1;
			goto cleanup;
		}

		/* readlink(2) won't NUL-terminate for us */
		target.ptr[ret] = '\0';
		target.size = ret;

		root = git_fs_path_root(target.ptr);
		if (root >= 0) {
			if ((error = git_str_sets(&curpath, target.ptr)) < 0)
				goto cleanup;
		} else {
			git_str dir = GIT_STR_INIT;

			if ((error = git_fs_path_dirname_r(&dir, curpath.ptr)) < 0)
				goto cleanup;

			git_str_swap(&curpath, &dir);
			git_str_dispose(&dir);

			if ((error = git_fs_path_apply_relative(&curpath, target.ptr)) < 0)
				goto cleanup;
		}
	}

	git_error_set(GIT_ERROR_INVALID, "maximum symlink depth reached");
	error = -1;

cleanup:
	git_str_dispose(&curpath);
	git_str_dispose(&target);
	return error;
}

int git_filebuf_open(git_filebuf *file, const char *path, int flags, mode_t mode)
{
	return git_filebuf_open_withsize(file, path, flags, mode, WRITE_BUFFER_SIZE);
}

int git_filebuf_open_withsize(git_filebuf *file, const char *path, int flags, mode_t mode, size_t size)
{
	int compression, error = -1;
	size_t path_len, alloc_len;

	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(path);
	GIT_ASSERT(file->buffer == NULL);

	memset(file, 0x0, sizeof(git_filebuf));

	if (flags & GIT_FILEBUF_DO_NOT_BUFFER)
		file->do_not_buffer = true;

	if (flags & GIT_FILEBUF_FSYNC)
		file->do_fsync = true;

	file->buf_size = size;
	file->buf_pos = 0;
	file->fd = -1;
	file->last_error = BUFERR_OK;

	/* Allocate the main cache buffer */
	if (!file->do_not_buffer) {
		file->buffer = git__malloc(file->buf_size);
		GIT_ERROR_CHECK_ALLOC(file->buffer);
	}

	/* If we are hashing on-write, allocate a new hash context */
	if (flags & GIT_FILEBUF_HASH_SHA1) {
		file->compute_digest = 1;

		if (git_hash_ctx_init(&file->digest, GIT_HASH_ALGORITHM_SHA1) < 0)
			goto cleanup;
	} else if (flags & GIT_FILEBUF_HASH_SHA256) {
		file->compute_digest = 1;

		if (git_hash_ctx_init(&file->digest, GIT_HASH_ALGORITHM_SHA256) < 0)
			goto cleanup;
	}

	compression = flags >> GIT_FILEBUF_DEFLATE_SHIFT;

	/* If we are deflating on-write, */
	if (compression != 0) {
		/* Initialize the ZLib stream */
		if (deflateInit(&file->zs, compression) != Z_OK) {
			git_error_set(GIT_ERROR_ZLIB, "failed to initialize zlib");
			goto cleanup;
		}

		/* Allocate the Zlib cache buffer */
		file->z_buf = git__malloc(file->buf_size);
		GIT_ERROR_CHECK_ALLOC(file->z_buf);

		/* Never flush */
		file->flush_mode = Z_NO_FLUSH;
		file->write = &write_deflate;
	} else {
		file->write = &write_normal;
	}

	/* If we are writing to a temp file */
	if (flags & GIT_FILEBUF_TEMPORARY) {
		git_str tmp_path = GIT_STR_INIT;

		/* Open the file as temporary for locking */
		file->fd = git_futils_mktmp(&tmp_path, path, mode);

		if (file->fd < 0) {
			git_str_dispose(&tmp_path);
			goto cleanup;
		}
		file->fd_is_open = true;
		file->created_lock = true;

		/* No original path */
		file->path_original = NULL;
		file->path_lock = git_str_detach(&tmp_path);
		GIT_ERROR_CHECK_ALLOC(file->path_lock);
	} else {
		git_str resolved_path = GIT_STR_INIT;

		if ((error = resolve_symlink(&resolved_path, path)) < 0)
			goto cleanup;

		/* Save the original path of the file */
		path_len = resolved_path.size;
		file->path_original = git_str_detach(&resolved_path);

		/* create the locking path by appending ".lock" to the original */
		GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, path_len, GIT_FILELOCK_EXTLENGTH);
		file->path_lock = git__malloc(alloc_len);
		GIT_ERROR_CHECK_ALLOC(file->path_lock);

		memcpy(file->path_lock, file->path_original, path_len);
		memcpy(file->path_lock + path_len, GIT_FILELOCK_EXTENSION, GIT_FILELOCK_EXTLENGTH);

		if (git_fs_path_isdir(file->path_original)) {
			git_error_set(GIT_ERROR_FILESYSTEM, "path '%s' is a directory", file->path_original);
			error = GIT_EDIRECTORY;
			goto cleanup;
		}

		/* open the file for locking */
		if ((error = lock_file(file, flags, mode)) < 0)
			goto cleanup;

		file->created_lock = true;
	}

	return 0;

cleanup:
	git_filebuf_cleanup(file);
	return error;
}

int git_filebuf_hash(unsigned char *out, git_filebuf *file)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(file->compute_digest);

	flush_buffer(file);

	if (verify_last_error(file) < 0)
		return -1;

	git_hash_final(out, &file->digest);
	git_hash_ctx_cleanup(&file->digest);
	file->compute_digest = 0;

	return 0;
}

int git_filebuf_commit_at(git_filebuf *file, const char *path)
{
	git__free(file->path_original);
	file->path_original = git__strdup(path);
	GIT_ERROR_CHECK_ALLOC(file->path_original);

	return git_filebuf_commit(file);
}

int git_filebuf_commit(git_filebuf *file)
{
	/* temporary files cannot be committed */
	GIT_ASSERT_ARG(file);
	GIT_ASSERT(file->path_original);

	file->flush_mode = Z_FINISH;
	flush_buffer(file);

	if (verify_last_error(file) < 0)
		goto on_error;

	file->fd_is_open = false;

	if (file->do_fsync && p_fsync(file->fd) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to fsync '%s'", file->path_lock);
		goto on_error;
	}

	if (p_close(file->fd) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to close file at '%s'", file->path_lock);
		goto on_error;
	}

	file->fd = -1;

	if (p_rename(file->path_lock, file->path_original) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to rename lockfile to '%s'", file->path_original);
		goto on_error;
	}

	if (file->do_fsync && git_futils_fsync_parent(file->path_original) < 0)
		goto on_error;

	file->did_rename = true;

	git_filebuf_cleanup(file);
	return 0;

on_error:
	git_filebuf_cleanup(file);
	return -1;
}

GIT_INLINE(void) add_to_cache(git_filebuf *file, const void *buf, size_t len)
{
	memcpy(file->buffer + file->buf_pos, buf, len);
	file->buf_pos += len;
}

int git_filebuf_write(git_filebuf *file, const void *buff, size_t len)
{
	const unsigned char *buf = buff;

	ENSURE_BUF_OK(file);

	if (file->do_not_buffer)
		return file->write(file, (void *)buff, len);

	for (;;) {
		size_t space_left = file->buf_size - file->buf_pos;

		/* cache if it's small */
		if (space_left > len) {
			add_to_cache(file, buf, len);
			return 0;
		}

		add_to_cache(file, buf, space_left);
		if (flush_buffer(file) < 0)
			return -1;

		len -= space_left;
		buf += space_left;
	}
}

int git_filebuf_reserve(git_filebuf *file, void **buffer, size_t len)
{
	size_t space_left = file->buf_size - file->buf_pos;

	*buffer = NULL;

	ENSURE_BUF_OK(file);

	if (len > file->buf_size) {
		file->last_error = BUFERR_MEM;
		return -1;
	}

	if (space_left <= len) {
		if (flush_buffer(file) < 0)
			return -1;
	}

	*buffer = (file->buffer + file->buf_pos);
	file->buf_pos += len;

	return 0;
}

int git_filebuf_printf(git_filebuf *file, const char *format, ...)
{
	va_list arglist;
	size_t space_left, len, alloclen;
	int written, res;
	char *tmp_buffer;

	ENSURE_BUF_OK(file);

	space_left = file->buf_size - file->buf_pos;

	do {
		va_start(arglist, format);
		written = p_vsnprintf((char *)file->buffer + file->buf_pos, space_left, format, arglist);
		va_end(arglist);

		if (written < 0) {
			file->last_error = BUFERR_MEM;
			return -1;
		}

		len = written;
		if (len + 1 <= space_left) {
			file->buf_pos += len;
			return 0;
		}

		if (flush_buffer(file) < 0)
			return -1;

		space_left = file->buf_size - file->buf_pos;

	} while (len + 1 <= space_left);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, len, 1) ||
		!(tmp_buffer = git__malloc(alloclen))) {
		file->last_error = BUFERR_MEM;
		return -1;
	}

	va_start(arglist, format);
	written = p_vsnprintf(tmp_buffer, len + 1, format, arglist);
	va_end(arglist);

	if (written < 0) {
		git__free(tmp_buffer);
		file->last_error = BUFERR_MEM;
		return -1;
	}

	res = git_filebuf_write(file, tmp_buffer, len);
	git__free(tmp_buffer);

	return res;
}

int git_filebuf_stats(time_t *mtime, size_t *size, git_filebuf *file)
{
	int res;
	struct stat st;

	if (file->fd_is_open)
		res = p_fstat(file->fd, &st);
	else
		res = p_stat(file->path_original, &st);

	if (res < 0) {
		git_error_set(GIT_ERROR_OS, "could not get stat info for '%s'",
			file->path_original);
		return res;
	}

	if (mtime)
		*mtime = st.st_mtime;
	if (size)
		*size = (size_t)st.st_size;

	return 0;
}
