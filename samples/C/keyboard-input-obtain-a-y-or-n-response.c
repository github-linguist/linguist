#include <stdio.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

void set_mode(int want_key)
{
	static struct termios old, new;
	if (!want_key) {
		tcsetattr(STDIN_FILENO, TCSANOW, &old);
		return;
	}

	tcgetattr(STDIN_FILENO, &old);
	new = old;
	new.c_lflag &= ~(ICANON);
	tcsetattr(STDIN_FILENO, TCSANOW, &new);
}

int get_key(int no_timeout)
{
	int c = 0;
	struct timeval tv;
	fd_set fs;
	tv.tv_usec = tv.tv_sec = 0;

	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);

	select(STDIN_FILENO + 1, &fs, 0, 0, no_timeout ? 0 : &tv);
	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}

int main()
{
	int c;
	while(1) {
		set_mode(1);
		while (get_key(0)); /* clear buffer */
		printf("Prompt again [Y/N]? ");
		fflush(stdout);

		c = get_key(1);
		if (c == 'Y' || c == 'y') {
			printf("\n");
			continue;
		}

		if (c == 'N' || c == 'n') {
			printf("\nDone\n");
			break;
		}

		printf("\nYes or no?\n");
	}

	return 0;
}
