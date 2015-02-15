#include <stdio.h>
#include <stdlib.h>

/* Flip a coin, 10 times. */
int
main()
{
	int i;
	srand(time(NULL));
	for (i = 0; i < 10; i++)
		puts((rand() % 2) ? "heads" : "tails");
	return 0;
}
