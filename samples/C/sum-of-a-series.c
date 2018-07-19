#include <stdio.h>

double Invsqr(double n)
{
	return 1 / (n*n);
}

int main (int argc, char *argv[])
{
	int i, start = 1, end = 1000;
	double sum = 0.0;
	
	for( i = start; i <= end; i++)
		sum += Invsqr((double)i);
	
	printf("%16.14f\n", sum);
	
	return 0;
}
