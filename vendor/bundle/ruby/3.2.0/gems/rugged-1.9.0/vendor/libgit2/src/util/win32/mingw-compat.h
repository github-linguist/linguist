/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_mingw_compat_h__
#define INCLUDE_win32_mingw_compat_h__

#if defined(__MINGW32__)

#undef stat

#if _WIN32_WINNT < 0x0600 && !defined(__MINGW64_VERSION_MAJOR)
#undef MemoryBarrier
void __mingworg_MemoryBarrier(void);
#define MemoryBarrier __mingworg_MemoryBarrier
#define VOLUME_NAME_DOS 0x0
#endif

#endif

#endif
