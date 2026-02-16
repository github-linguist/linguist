/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_version_h__
#define INCLUDE_win32_version_h__

#include <windows.h>

GIT_INLINE(int) git_has_win32_version(int major, int minor, int service_pack)
{
	OSVERSIONINFOEX version_test = {0};
	DWORD version_test_mask;
	DWORDLONG version_condition_mask = 0;
	
	version_test.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
	version_test.dwMajorVersion = major;
	version_test.dwMinorVersion = minor;
	version_test.wServicePackMajor = (WORD)service_pack;
	version_test.wServicePackMinor = 0;

	version_test_mask = (VER_MAJORVERSION | VER_MINORVERSION | VER_SERVICEPACKMAJOR | VER_SERVICEPACKMINOR);

	VER_SET_CONDITION(version_condition_mask, VER_MAJORVERSION, VER_GREATER_EQUAL);
	VER_SET_CONDITION(version_condition_mask, VER_MINORVERSION, VER_GREATER_EQUAL);
	VER_SET_CONDITION(version_condition_mask, VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL);
	VER_SET_CONDITION(version_condition_mask, VER_SERVICEPACKMINOR, VER_GREATER_EQUAL);

	if (!VerifyVersionInfo(&version_test, version_test_mask, version_condition_mask))
		return 0;

	return 1;
}

#endif
