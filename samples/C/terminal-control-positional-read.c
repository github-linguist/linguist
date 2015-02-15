#include <windows.h>
#include <wchar.h>

int
main()
{
	CONSOLE_SCREEN_BUFFER_INFO info;
	COORD pos;
	HANDLE conout;
	long len;
	wchar_t c;

	/* Create a handle to the console screen. */
	conout = CreateFileW(L"CONOUT$", GENERIC_READ | GENERIC_WRITE,
	    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
	    0, NULL);
	if (conout == INVALID_HANDLE_VALUE)
		return 1;

	/* Where is the display window? */
	if (GetConsoleScreenBufferInfo(conout, &info) == 0)
		return 1;

	/* c = character at position. */
	pos.X = info.srWindow.Left + 3;  /* Column */
	pos.Y = info.srWindow.Top  + 6;  /* Row */
	if (ReadConsoleOutputCharacterW(conout, &c, 1, pos, &len) == 0 ||
	    len <= 0)
		return 1;

	wprintf(L"Character at (3, 6) had been '%lc'\n", c);
	return 0;
}
