/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#include "map.h"
#include <errno.h>

#ifndef NO_MMAP

static DWORD get_page_size(void)
{
	static DWORD page_size;
	SYSTEM_INFO sys;

	if (!page_size) {
		GetSystemInfo(&sys);
		page_size = sys.dwPageSize;
	}

	return page_size;
}

static DWORD get_allocation_granularity(void)
{
	static DWORD granularity;
	SYSTEM_INFO sys;

	if (!granularity) {
		GetSystemInfo(&sys);
		granularity = sys.dwAllocationGranularity;
	}

	return granularity;
}

int git__page_size(size_t *page_size)
{
	*page_size = get_page_size();
	return 0;
}

int git__mmap_alignment(size_t *page_size)
{
	*page_size = get_allocation_granularity();
	return 0;
}

int p_mmap(git_map *out, size_t len, int prot, int flags, int fd, off64_t offset)
{
	HANDLE fh = (HANDLE)_get_osfhandle(fd);
	DWORD alignment = get_allocation_granularity();
	DWORD fmap_prot = 0;
	DWORD view_prot = 0;
	DWORD off_low = 0;
	DWORD off_hi = 0;
	off64_t page_start;
	off64_t page_offset;

	GIT_MMAP_VALIDATE(out, len, prot, flags);

	out->data = NULL;
	out->len = 0;
	out->fmh = NULL;

	if (fh == INVALID_HANDLE_VALUE) {
		errno = EBADF;
		git_error_set(GIT_ERROR_OS, "failed to mmap. Invalid handle value");
		return -1;
	}

	if (prot & GIT_PROT_WRITE)
		fmap_prot |= PAGE_READWRITE;
	else if (prot & GIT_PROT_READ)
		fmap_prot |= PAGE_READONLY;

	if (prot & GIT_PROT_WRITE)
		view_prot |= FILE_MAP_WRITE;
	if (prot & GIT_PROT_READ)
		view_prot |= FILE_MAP_READ;

	page_start = (offset / alignment) * alignment;
	page_offset = offset - page_start;

	if (page_offset != 0) { /* offset must be multiple of the allocation granularity */
		errno = EINVAL;
		git_error_set(GIT_ERROR_OS, "failed to mmap. Offset must be multiple of allocation granularity");
		return -1;
	}

	out->fmh = CreateFileMapping(fh, NULL, fmap_prot, 0, 0, NULL);
	if (!out->fmh || out->fmh == INVALID_HANDLE_VALUE) {
		git_error_set(GIT_ERROR_OS, "failed to mmap. Invalid handle value");
		out->fmh = NULL;
		return -1;
	}

	off_low = (DWORD)(page_start);
	off_hi = (DWORD)(page_start >> 32);
	out->data = MapViewOfFile(out->fmh, view_prot, off_hi, off_low, len);
	if (!out->data) {
		git_error_set(GIT_ERROR_OS, "failed to mmap. No data written");
		CloseHandle(out->fmh);
		out->fmh = NULL;
		return -1;
	}
	out->len = len;

	return 0;
}

int p_munmap(git_map *map)
{
	int error = 0;

	GIT_ASSERT_ARG(map);

	if (map->data) {
		if (!UnmapViewOfFile(map->data)) {
			git_error_set(GIT_ERROR_OS, "failed to munmap. Could not unmap view of file");
			error = -1;
		}
		map->data = NULL;
	}

	if (map->fmh) {
		if (!CloseHandle(map->fmh)) {
			git_error_set(GIT_ERROR_OS, "failed to munmap. Could not close handle");
			error = -1;
		}
		map->fmh = NULL;
	}

	return error;
}

#endif
