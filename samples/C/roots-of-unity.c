#include <stdio.h>
#include <math.h>

int main()
{
	double a, c, s, PI2 = atan2(1, 1) * 8;
	int n, i;

	for (n = 1; n < 10; n++) for (i = 0; i < n; i++) {
		c = s = 0;
		if (!i )		c =  1;
		else if(n == 4 * i)	s =  1;
		else if(n == 2 * i)	c = -1;
		else if(3 * n == 4 * i)	s = -1;
		else
			a = i * PI2 / n, c = cos(a), s = sin(a);

		if (c) printf("%.2g", c);
		printf(s == 1 ? "i" : s == -1 ? "-i" : s ? "%+.2gi" : "", s);
		printf(i == n - 1 ?"\n":",  ");
	}

	return 0;
}
