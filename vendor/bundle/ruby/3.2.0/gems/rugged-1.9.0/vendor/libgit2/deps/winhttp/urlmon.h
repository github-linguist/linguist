/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#if defined(__MINGW_VERSION) || defined(__MINGW32_VERSION)

#ifndef __CUSTOM_URLMON_H
#define __CUSTOM_URLMON_H

typedef struct IInternetSecurityManager IInternetSecurityManager;

typedef struct IInternetSecurityManagerVtbl
{
	HRESULT(STDMETHODCALLTYPE *QueryInterface)(IInternetSecurityManager *, REFIID, void **);
	ULONG(STDMETHODCALLTYPE *AddRef)(IInternetSecurityManager *);
	ULONG(STDMETHODCALLTYPE *Release)(IInternetSecurityManager *);
	LPVOID SetSecuritySite;
	LPVOID GetSecuritySite;
	HRESULT(STDMETHODCALLTYPE *MapUrlToZone)(IInternetSecurityManager *, LPCWSTR, DWORD *, DWORD);
	LPVOID GetSecurityId;
	LPVOID ProcessUrlAction;
	LPVOID QueryCustomPolicy;
	LPVOID SetZoneMapping;
	LPVOID GetZoneMappings;
} IInternetSecurityManagerVtbl;

struct IInternetSecurityManager
{
	CONST_VTBL struct IInternetSecurityManagerVtbl *lpVtbl;
};

#define URLZONE_LOCAL_MACHINE 0
#define URLZONE_INTRANET      1
#define URLZONE_TRUSTED       2

#endif /* __CUSTOM_URLMON_H */

#else

#include_next <urlmon.h>

#endif
