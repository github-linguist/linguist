// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/windef.c3
// Permalink: https://github.com/c3lang/c3c/blob/c9e1140/lib/std/os/win32/windef.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32 @if(env::WIN32);

struct Win32_RECT
{
	Win32_LONG left;
	Win32_LONG top;
	Win32_LONG right;
	Win32_LONG bottom;
}

struct Win32_POINT
{
	Win32_LONG x;
	Win32_LONG y;
}

struct Win32_SIZE
{
	Win32_LONG cx;
	Win32_LONG cy;
}

def Win32_PSIZE = Win32_SIZE*;
def Win32_NPSIZE = Win32_SIZE*;
def Win32_LPSIZE = Win32_SIZE*;

def Win32_PPOINT = Win32_POINT*;
def Win32_NPOINT = Win32_POINT*;
def Win32_LPOINT = Win32_POINT*;

def Win32_PRECT = Win32_RECT*;
def Win32_NPRECT = Win32_RECT*;
def Win32_LPRECT = Win32_RECT*;
