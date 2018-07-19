#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define foreach(a, b, c) for (int a = b; a < c; a++)
#define for_i foreach(i, 0, n)
#define for_j foreach(j, 0, n)
#define for_k foreach(k, 0, n)
#define for_ij for_i for_j
#define for_ijk for_ij for_k
#define _dim int n
#define _swap(x, y) { typeof(x) tmp = x; x = y; y = tmp; }
#define _sum_k(a, b, c, s) { s = 0; foreach(k, a, b) s+= c; }

typedef double **mat;

#define _zero(a) mat_zero(a, n)
void mat_zero(mat x, int n) { for_ij x[i][j] = 0; }

#define _new(a) a = mat_new(n)
mat mat_new(_dim)
{
	mat x = malloc(sizeof(double*) * n);
	x[0]  = malloc(sizeof(double) * n * n);

	for_i x[i] = x[0] + n * i;
	_zero(x);

	return x;
}

#define _copy(a) mat_copy(a, n)
mat mat_copy(void *s, _dim)
{
	mat x = mat_new(n);
	for_ij x[i][j] = ((double (*)[n])s)[i][j];
	return x;
}

#define _del(x) mat_del(x)
void mat_del(mat x) { free(x[0]); free(x); }

#define _QUOT(x) #x
#define QUOTE(x) _QUOT(x)
#define _show(a) printf(QUOTE(a)" =");mat_show(a, 0, n)
void mat_show(mat x, char *fmt, _dim)
{
	if (!fmt) fmt = "%8.4g";
	for_i {
		printf(i ? "      " : " [ ");
		for_j {
			printf(fmt, x[i][j]);
			printf(j < n - 1 ? "  " : i == n - 1 ? " ]\n" : "\n");
		}
	}
}

#define _mul(a, b) mat_mul(a, b, n)
mat mat_mul(mat a, mat b, _dim)
{
	mat c = _new(c);
	for_ijk c[i][j] += a[i][k] * b[k][j];
	return c;
}

#define _pivot(a, b) mat_pivot(a, b, n)
void mat_pivot(mat a, mat p, _dim)
{
	for_ij { p[i][j] = (i == j); }
	for_i  {
		int max_j = i;
		foreach(j, i, n)
			if (fabs(a[j][i]) > fabs(a[max_j][i])) max_j = j;

		if (max_j != i)
			for_k { _swap(p[i][k], p[max_j][k]); }
	}
}

#define _LU(a, l, u, p) mat_LU(a, l, u, p, n)
void mat_LU(mat A, mat L, mat U, mat P, _dim)
{
	_zero(L); _zero(U);
	_pivot(A, P);

	mat Aprime = _mul(P, A);

	for_i  { L[i][i] = 1; }
	for_ij {
		double s;
		if (j <= i) {
			_sum_k(0, j, L[j][k] * U[k][i], s)
			U[j][i] = Aprime[j][i] - s;
		}
		if (j >= i) {
			_sum_k(0, i, L[j][k] * U[k][i], s);
			L[j][i] = (Aprime[j][i] - s) / U[i][i];
		}
	}

	_del(Aprime);
}

double A3[][3] = {{ 1, 3, 5 }, { 2, 4, 7 }, { 1, 1, 0 }};
double A4[][4] = {{11, 9, 24, 2}, {1, 5, 2, 6}, {3, 17, 18, 1}, {2, 5, 7, 1}};

int main()
{
	int n = 3;
	mat A, L, P, U;

	_new(L); _new(P); _new(U);
	A = _copy(A3);
	_LU(A, L, U, P);
	_show(A); _show(L); _show(U); _show(P);
	_del(A);  _del(L);  _del(U);  _del(P);

	printf("\n");

	n = 4;

	_new(L); _new(P); _new(U);
	A = _copy(A4);
	_LU(A, L, U, P);
	_show(A); _show(L); _show(U); _show(P);
	_del(A);  _del(L);  _del(U);  _del(P);

	return 0;
}
