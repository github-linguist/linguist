#include <stdio.h>

int main()
{
	fprintf(stderr, "Goodbye, ");
	fputs("World!\n", stderr);

	return 0;
}
