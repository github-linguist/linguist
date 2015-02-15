#include<stdio.h>

typedef struct{
	float i,j,k;
	}Vector;

Vector a = {3, 4, 5},b = {4, 3, 5},c = {-5, -12, -13};

float dotProduct(Vector a, Vector b)
{
	return a.i*b.i+a.j*b.j+a.k*b.k;
}

Vector crossProduct(Vector a,Vector b)
{
	Vector c = {a.j*b.k - a.k*b.j, a.k*b.i - a.i*b.k, a.i*b.j - a.j*b.i};
	
	return c;
}

float scalarTripleProduct(Vector a,Vector b,Vector c)
{
	return dotProduct(a,crossProduct(b,c));
}

Vector vectorTripleProduct(Vector a,Vector b,Vector c)
{
	return crossProduct(a,crossProduct(b,c));
}

void printVector(Vector a)
{
	printf("( %f, %f, %f)",a.i,a.j,a.k);
}

int main()
{
	printf("\n a = "); printVector(a);
	printf("\n b = "); printVector(b);
	printf("\n c = "); printVector(c);
	printf("\n a . b = %f",dotProduct(a,b));
	printf("\n a x b = "); printVector(crossProduct(a,b));
	printf("\n a . (b x c) = %f",scalarTripleProduct(a,b,c));
	printf("\n a x (b x c) = "); printVector(vectorTripleProduct(a,b,c));
	
	return 0;
}
