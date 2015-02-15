#include <stdio.h>
#include <stdlib.h>

int main(int c, char *v[])
{
	int days[] = {31,29,31,30,31,30,31,31,30,31,30,31};
	int m, y, w;

	if (c < 2 || (y = atoi(v[1])) <= 1700) return 1;
 	days[1] -= (y % 4) || (!(y % 100) && (y % 400));
	w = y * 365 + (y - 1) / 4 - (y - 1) / 100 + (y - 1) / 400 + 6;

	for(m = 0; m < 12; m++) {
		w = (w + days[m]) % 7;
		printf("%d-%02d-%d\n", y, m + 1,
			days[m] + (w < 5 ? -2 : 5) - w);
	}

	return 0;
}
