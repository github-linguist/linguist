#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_N 20
#define TIMES 1000000

double factorial(int n) {
	double f = 1;
	int i;
	for (i = 1; i <= n; i++) f *= i;
	return f;
}

double expected(int n) {
	double sum = 0;
	int i;
	for (i = 1; i <= n; i++)
		sum += factorial(n) / pow(n, i) / factorial(n - i);
	return sum;
}

int randint(int n) {
	int r, rmax = RAND_MAX / n * n;
	while ((r = rand()) >= rmax);
	return r / (RAND_MAX / n);
}

int test(int n, int times) {
	int i, count = 0;
	for (i = 0; i < times; i++) {
		int x = 1, bits = 0;
		while (!(bits & x)) {
			count++;
			bits |= x;
			x = 1 << randint(n);
		}
	}
	return count;
}

int main(void) {
	srand(time(0));
	puts(" n\tavg\texp.\tdiff\n-------------------------------");

	int n;
	for (n = 1; n <= MAX_N; n++) {
		int cnt = test(n, TIMES);
		double avg = (double)cnt / TIMES;
		double theory = expected(n);
		double diff = (avg / theory - 1) * 100;
		printf("%2d %8.4f %8.4f %6.3f%%\n", n, avg, theory, diff);
	}
	return 0;
}
