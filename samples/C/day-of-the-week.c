#include <stdio.h>

/* Calculate day of week in proleptic Gregorian calendar. Sunday == 0. */
int wday(int year, int month, int day)
{
	int adjustment, mm, yy;

	adjustment = (14 - month) / 12;
	mm = month + 12 * adjustment - 2;
	yy = year - adjustment;
	return (day + (13 * mm - 1) / 5 +
		yy + yy / 4 - yy / 100 + yy / 400) % 7;
}

int main()
{
	int y;

	for (y = 2008; y <= 2121; y++) {
		if (wday(y, 12, 25) == 0) printf("%04d-12-25\n", y);
	}

	return 0;
}
