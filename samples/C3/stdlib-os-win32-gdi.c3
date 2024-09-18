// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/gdi.c3
// Permalink: https://github.com/c3lang/c3c/blob/c9e1140/lib/std/os/win32/gdi.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32 @if(env::WIN32);

extern fn Win32_HBRUSH createSolidBrush(Win32_COLORREF) @extern("CreateSolidBrush");
extern fn Win32_COLORREF setTextColor(Win32_HDC, Win32_COLORREF) @extern("SetTextColor");
extern fn CInt setBkMode(Win32_HDC, CInt) @extern("SetBkMode");
extern fn Win32_BOOL textOut(Win32_HDC, CInt, CInt, Win32_LPCWSTR, CInt) @extern("TextOutW");