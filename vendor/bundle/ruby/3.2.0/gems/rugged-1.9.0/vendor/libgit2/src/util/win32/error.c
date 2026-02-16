/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "error.h"

#include "utf-conv.h"

#ifdef GIT_WINHTTP
# include <winhttp.h>
#endif

char *git_win32_get_error_message(DWORD error_code)
{
	LPWSTR lpMsgBuf = NULL;
	HMODULE hModule = NULL;
	char *utf8_msg = NULL;
	DWORD dwFlags =
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS;

	if (!error_code)
		return NULL;

#ifdef GIT_WINHTTP
	/* Errors raised by WinHTTP are not in the system resource table */
	if (error_code >= WINHTTP_ERROR_BASE &&
		error_code <= WINHTTP_ERROR_LAST)
		hModule = GetModuleHandleW(L"winhttp");
#endif

	GIT_UNUSED(hModule);

	if (hModule)
		dwFlags |= FORMAT_MESSAGE_FROM_HMODULE;
	else
		dwFlags |= FORMAT_MESSAGE_FROM_SYSTEM;

	if (FormatMessageW(dwFlags, hModule, error_code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPWSTR)&lpMsgBuf, 0, NULL)) {
		/* Convert the message to UTF-8. If this fails, we will
		 * return NULL, which is a condition expected by the caller */
		if (git_utf8_from_16_alloc(&utf8_msg, lpMsgBuf) < 0)
			utf8_msg = NULL;

		LocalFree(lpMsgBuf);
	}

	return utf8_msg;
}
