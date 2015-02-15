#include <stdio.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_multifit.h>

double w[] = {	52.21, 53.12, 54.48, 55.84, 57.20,
		58.57, 59.93, 61.29, 63.11, 64.47,
		66.28, 68.10, 69.92, 72.19, 74.46 };
double h[] = {	1.47, 1.50, 1.52, 1.55, 1.57,
		1.60, 1.63, 1.65, 1.68, 1.70,
		1.73, 1.75, 1.78, 1.80, 1.83	};

int main()
{
	int n = sizeof(h)/sizeof(double);
	gsl_matrix *X = gsl_matrix_calloc(n, 3);
	gsl_vector *Y = gsl_vector_alloc(n);
	gsl_vector *beta = gsl_vector_alloc(3);

	for (int i = 0; i < n; i++) {
		gsl_vector_set(Y, i, w[i]);

		gsl_matrix_set(X, i, 0, 1);
		gsl_matrix_set(X, i, 1, h[i]);
		gsl_matrix_set(X, i, 2, h[i] * h[i]);
	}

	double chisq;
	gsl_matrix *cov = gsl_matrix_alloc(3, 3);
	gsl_multifit_linear_workspace * wspc = gsl_multifit_linear_alloc(n, 3);
	gsl_multifit_linear(X, Y, beta, cov, &chisq, wspc);

	printf("Beta:");
	for (int i = 0; i < 3; i++)
		printf("  %g", gsl_vector_get(beta, i));
	printf("\n");

	gsl_matrix_free(X);
	gsl_matrix_free(cov);
	gsl_vector_free(Y);
	gsl_vector_free(beta);
	gsl_multifit_linear_free(wspc);

}
