#include <stdio.h>
#include <unistd.h>

int main()
{
	int i;
	printf("\033[?1049h\033[H");
	printf("Alternate screen buffer\n");
	for (i = 5; i; i--) {
		printf("\rgoing back in %d...", i);
		fflush(stdout);
		sleep(1);
	}
	printf("\033[?1049l");

	return 0;
}
