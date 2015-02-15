#include <math.h>
#include <stdio.h>

double f(double x)
{
    return x*x*x-3.0*x*x +2.0*x;
}

double secant( double xA, double xB, double(*f)(double) )
{
    double e = 1.0e-12;
    double fA, fB;
    double d;
    int i;
    int limit = 50;

    fA=(*f)(xA);
    for (i=0; i<limit; i++) {
        fB=(*f)(xB);
        d = (xB - xA) / (fB - fA) * fB;
        if (fabs(d) < e)
            break;
        xA = xB;
        fA = fB;
        xB -= d;
    }
    if (i==limit) {
        printf("Function is not converging near (%7.4f,%7.4f).\n", xA,xB);
        return -99.0;
    }
    return xB;
}

int main(int argc, char *argv[])
{
    double step = 1.0e-2;
    double e = 1.0e-12;
    double x = -1.032;		// just so we use secant method
    double xx, value;

    int s = (f(x)> 0.0);

    while (x < 3.0) {
        value = f(x);
        if (fabs(value) < e) {
            printf("Root found at x= %12.9f\n", x);
            s = (f(x+.0001)>0.0);
        }
        else if ((value > 0.0) != s) {
            xx = secant(x-step, x,&f);
            if (xx != -99.0)   // -99 meaning secand method failed
                printf("Root found at x= %12.9f\n", xx);
            else
                printf("Root found near x= %7.4f\n", x);
            s = (f(x+.0001)>0.0);
        }
        x += step;
    }
    return 0;
}
