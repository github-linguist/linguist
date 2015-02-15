#include <stdio.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>

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

int get_key()
{
	int c = 0;
	fd_set fs;

	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);
	select(STDIN_FILENO + 1, &fs, 0, 0, 0);

	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}

int main()
{
	int c = 0;
	while (c != 'n') {
		set_mode(1);

		/* flush pending input so we won't format the hardrive
		   because user accidentally typed 'y' before we even prompted */
		tcflush(STDIN_FILENO, TCIFLUSH);

		printf("Show this prompt again [Yes/No/Ignore you]? ");
		fflush(stdout);

		switch(c = tolower(get_key())) {
		case 'y':	putchar('\n');
				break;

		case 'n':	printf("\nDone\n");
				break;

		case 'i':	puts("\nI'll ignore keys for 5 seconds");
				sleep(5);
				putchar('\n');
				break;
		default:
				puts("\nAssume that was the cat.");
		}
	}

	return 0;
}
