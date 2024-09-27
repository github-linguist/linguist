// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/files.c3
// Permalink: https://github.com/c3lang/c3c/blob/c9e1140/lib/std/os/win32/files.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32 @if(env::WIN32);
import libc;
enum Win32_GET_FILEEX_INFO_LEVELS
{
	STANDARD,
	MAX,
}

struct Win32_FILE_ATTRIBUTE_DATA
{
	Win32_DWORD    dwFileAttributes;
	Win32_FILETIME ftCreationTime;
	Win32_FILETIME ftLastAccessTime;
	Win32_FILETIME ftLastWriteTime;
	Win32_DWORD    nFileSizeHigh;
	Win32_DWORD    nFileSizeLow;
}

const MAX_PATH = 260;

struct Win32_WIN32_FIND_DATAW
{
	Win32_DWORD         dwFileAttributes;
	Win32_FILETIME      ftCreationTime;
	Win32_FILETIME      ftLastAccessTime;
	Win32_FILETIME      ftLastWriteTime;
	Win32_DWORD         nFileSizeHigh;
	Win32_DWORD         nFileSizeLow;
	Win32_DWORD         dwReserved0;
	Win32_DWORD         dwReserved1;
	Win32_WCHAR[260]    cFileName;
	Win32_WCHAR[14]     cAlternateFileName;
	Win32_DWORD         dwFileType; // Obsolete. Do not use.
	Win32_DWORD         dwCreatorType; // Obsolete. Do not use
	Win32_WORD          wFinderFlags; // Obsolete. Do not use
}

def Win32_LPWIN32_FIND_DATAW = Win32_WIN32_FIND_DATAW*;

extern fn Win32_BOOL closeHandle(Win32_HANDLE) @extern("CloseHandle");
extern fn Win32_BOOL createPipe(Win32_PHANDLE hReadPipe, Win32_PHANDLE hWritePipe, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes, Win32_DWORD nSize) @extern("CreatePipe");
extern fn Win32_BOOL getFileAttributesExW(Win32_LPCWSTR, Win32_GET_FILEEX_INFO_LEVELS, Win32_LPVOID) @extern("GetFileAttributesExW");
extern fn Win32_BOOL pathFileExistsW(Win32_LPCWSTR) @extern("PathFileExistsW");
extern fn Win32_DWORD getTempPathW(Win32_DWORD nBufferLength, Win32_LPWSTR lpBuffer) @extern("GetTempPathW");
extern fn Win32_BOOL setCurrentDirectoryW(Win32_LPCTSTR buffer) @extern("SetCurrentDirectoryW");
extern fn Win32_BOOL removeDirectoryW(Win32_LPCWSTR lpPathName) @extern("RemoveDirectoryW");
extern fn Win32_BOOL createDirectoryW(Win32_LPCWSTR lpPathName, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes) @extern("CreateDirectoryW");
extern fn Win32_BOOL deleteFileW(Win32_LPCWSTR lpFileName) @extern("DeleteFileW");
extern fn Win32_HANDLE findFirstFileW(Win32_LPCWSTR lpFileName, Win32_LPWIN32_FIND_DATAW lpFindFileData) @extern("FindFirstFileW");
extern fn Win32_BOOL findNextFileW(Win32_HANDLE hFindFile, Win32_LPWIN32_FIND_DATAW lpFindFileData) @extern("FindNextFileW");
extern fn Win32_BOOL findClose(Win32_HANDLE hFindFile) @extern("FindClose");

const Win32_DWORD GENERIC_WRITE = 0x40000000;
const Win32_DWORD OPEN_EXISTING = 3;
const Win32_DWORD FILE_ATTRIBUTE_READONLY = 0x01;
const Win32_DWORD FILE_ATTRIBUTE_HIDDEN = 0x02;
const Win32_DWORD FILE_ATTRIBUTE_SYSTEM = 0x04;
const Win32_DWORD FILE_ATTRIBUTE_DIRECTORY = 0x10;
const Win32_DWORD FILE_ATTRIBUTE_ARCHIVE = 0x20;
const Win32_DWORD FILE_ATTRIBUTE_DEVICE = 0x40;
const Win32_DWORD FILE_ATTRIBUTE_NORMAL = 0x80;
const Win32_DWORD FILE_ATTRIBUTE_TEMPORARY = 0x100;
const Win32_DWORD FILE_ATTRIBUTE_SPARSE_FILE = 0x200;
const Win32_DWORD FILE_ATTRIBUTE_REPARSE_POINT = 0x400;
const Win32_DWORD FILE_ATTRIBUTE_COMPRESSED = 0x800;
const Win32_DWORD FILE_ATTRIBUTE_OFFLINE = 0x1000;
const Win32_DWORD FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 0x2000;
const Win32_DWORD FILE_ATTRIBUTE_ENCRYPTED = 0x4000;
const Win32_DWORD FILE_ATTRIBUTE_INTEGRITY_STREAM = 0x8000;
const Win32_DWORD FILE_ATTRIBUTE_VIRTUAL = 0x10000;
const Win32_DWORD FILE_ATTRIBUTE_NO_SCRUB_DATA = 0x20000;
const Win32_DWORD FILE_ATTRIBUTE_EA = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_PINNED = 0x80000;
const Win32_DWORD FILE_ATTRIBUTE_UNPINNED = 0x100000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_OPEN = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = 0x400000;

extern fn Win32_HANDLE createFileA(
	Win32_LPCSTR lpFileName,
	Win32_DWORD dwDesiredAccess,
	Win32_DWORD dwShareMode,
	Win32_LPSECURITY_ATTRIBUTES lpSecurityAttributes,
	Win32_DWORD dwCreationDisposition,
	Win32_DWORD dwFlagsAndAttributes,
	Win32_HANDLE hTemplateFile
) @extern("CreateFileA");
extern fn Win32_BOOL readFile(Win32_HANDLE hFile, Win32_LPVOID lpBuffer, Win32_DWORD nNumberOfBytesToRead,
  Win32_LPDWORD lpNumberOfBytesRead, Win32_LPOVERLAPPED lpOverlapped
) @extern("ReadFile");

extern fn WString _wgetcwd(Char16* buffer, int maxlen);
extern fn usz wcslen(WString str);

extern fn int _open_osfhandle(iptr osfhandle, int flags);
extern fn iptr _get_osfhandle(int fd);
extern fn CFile _fdopen(int fd, ZString mode);
extern fn CInt _access(ZString path, CInt mode);
extern fn CInt _waccess(WString path, CInt mode);

extern fn WString _wfullpath(WString absPath, WString relPath, usz maxLength);

/*
extern ulong _win32_GetCurrentDirectoryW(ulong, Char16* buffer) @extern("GetCurrentDirectoryW");
extern bool _win32_CreateSymbolicLinkW(WString symlink_file, WString target_file, ulong flags) @extern("CreateSymbolicLinkW");
extern bool _win32_CopyFileW(WString from_file, WString to_file, bool no_overwrite) @extern("CopyFileW");
extern ulong _win32_GetFullPathNameW(WString file_name, ulong buffer_len, Char16* buffer, WString* file_part) @extern("GetFullPathNameW");
*/
