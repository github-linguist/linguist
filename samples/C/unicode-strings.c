#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

/* wchar_t is the standard type for wide chars; what it is internally
 * depends on the compiler.
 */
wchar_t poker[] = L"♥♦♣♠";
wchar_t four_two[] = L"\x56db\x5341\x4e8c";

int main() {
	/* Set the locale to alert C's multibyte output routines */
	if (!setlocale(LC_CTYPE, "")) {
		fprintf(stderr, "Locale failure, check your env vars\n");
		return 1;
	}

#ifdef __STDC_ISO_10646__
	/* C99 compilers should understand these */
	printf("%lc\n", 0x2708);	/* ✈ */
	printf("%ls\n", poker);		/* ♥♦♣♠ */
	printf("%ls\n", four_two);	/* 四十二 */
#else
	/* oh well */
	printf("airplane\n");
	printf("club diamond club spade\n");
	printf("for ty two\n");
#endif
	return 0;
}
