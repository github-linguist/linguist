#include <stdio.h>
#include <stdlib.h>

int main()
{
	int junk, *junkp;

	/* Print an unitialized variable! */
	printf("junk: %d\n", junk);

	/* Follow a pointer to unitialized memory! */
	junkp = malloc(sizeof *junkp);
	if (junkp)
		printf("*junkp: %d\n", *junkp);
	return 0;
}
