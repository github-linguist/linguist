#include <stdio.h>

void table(const char *title, const char *mode)
{
	int f, b;
	printf("\n\033[1m%s\033[m\n bg\t fg\n", title);
	for (b = 40; b <= 107; b++) {
		if (b == 48) b = 100;
		printf("%3d\t\033[%s%dm", b, mode, b);
		for (f = 30; f <= 97; f++) {
			if (f == 38) f = 90;
			printf("\033[%dm%3d ", f, f);
		}
		puts("\033[m");
	}
}

int main(void)
{
	int fg, bg, blink, inverse;

	table("normal ( ESC[22m or ESC[m )", "22;");
	table("bold ( ESC[1m )", "1;");
	table("faint ( ESC[2m ), not well supported", "2;");
	table("italic ( ESC[3m ), not well supported", "3;");
	table("underline ( ESC[4m ), support varies", "4;");
	table("blink ( ESC[5m )", "5;");
	table("inverted ( ESC[7m )", "7;");
	return 0;
}
