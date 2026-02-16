/*
* Copyright (C) the libgit2 contributors. All rights reserved.
*
* This file is part of libgit2, distributed under the GNU GPL v2 with
* a Linking Exception. For full terms see the included COPYING file.
*/

#ifndef INCLUDE_win32_reparse_h__
#define INCLUDE_win32_reparse_h__

/* This structure is defined on MSDN at
* http://msdn.microsoft.com/en-us/library/windows/hardware/ff552012(v=vs.85).aspx
*
* It was formerly included in the Windows 2000 SDK and remains defined in
* MinGW, so we must define it with a silly name to avoid conflicting.
*/
typedef struct _GIT_REPARSE_DATA_BUFFER {
	ULONG  ReparseTag;
	USHORT ReparseDataLength;
	USHORT Reserved;
	union {
		struct {
			USHORT SubstituteNameOffset;
			USHORT SubstituteNameLength;
			USHORT PrintNameOffset;
			USHORT PrintNameLength;
			ULONG  Flags;
			WCHAR  PathBuffer[1];
		} SymbolicLink;
		struct {
			USHORT SubstituteNameOffset;
			USHORT SubstituteNameLength;
			USHORT PrintNameOffset;
			USHORT PrintNameLength;
			WCHAR  PathBuffer[1];
		} MountPoint;
		struct {
			UCHAR DataBuffer[1];
		} Generic;
	} ReparseBuffer;
} GIT_REPARSE_DATA_BUFFER;

#define REPARSE_DATA_HEADER_SIZE			8
#define REPARSE_DATA_MOUNTPOINT_HEADER_SIZE	8
#define REPARSE_DATA_UNION_SIZE				12

/* Missing in MinGW */
#ifndef FSCTL_GET_REPARSE_POINT
# define FSCTL_GET_REPARSE_POINT			0x000900a8
#endif

/* Missing in MinGW */
#ifndef FSCTL_SET_REPARSE_POINT
# define FSCTL_SET_REPARSE_POINT			0x000900a4
#endif

#endif
