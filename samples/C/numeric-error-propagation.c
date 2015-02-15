#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

typedef struct{
    double value;
    double delta;
}imprecise;

#define SQR(x) ((x) * (x))
imprecise imprecise_add(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value + b.value;
    ret.delta = sqrt(SQR(a.delta) + SQR(b.delta));
    return ret;
}

imprecise imprecise_mul(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value * b.value;
    ret.delta = sqrt(SQR(a.value * b.delta) + SQR(b.value * a.delta));
    return ret;
}

imprecise imprecise_div(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value / b.value;
    ret.delta = sqrt(SQR(a.value * b.delta) + SQR(b.value * a.delta)) / SQR(b.value);
    return ret;
}

imprecise imprecise_pow(imprecise a, double c)
{
    imprecise ret;
    ret.value = pow(a.value, c);
    ret.delta = fabs(ret.value * c * a.delta / a.value);
    return ret;
}

char* printImprecise(imprecise val)
{
	char principal[30],error[30],*string,sign[2];
	sign[0] = 241;    /* ASCII code for ±, technical notation for denoting errors */
	sign[1] = 00;
	
	sprintf(principal,"%f",val.value);
	sprintf(error,"%f",val.delta);
	
	string = (char*)malloc((strlen(principal)+1+strlen(error)+1)*sizeof(char));
	
	strcpy(string,principal);
	strcat(string,sign);
	strcat(string,error);
	
	return string;
}

int main(void) {
    imprecise x1 = {100, 1.1};
    imprecise y1 = {50, 1.2};
    imprecise x2 = {-200, 2.2};
    imprecise y2 = {-100, 2.3};
    imprecise d;

    d = imprecise_pow(imprecise_add(imprecise_pow(imprecise_add(x1, x2), 2),imprecise_pow(imprecise_add(y1, y2), 2)), 0.5);
    printf("Distance, d, between the following points :");
    printf("\n( x1, y1) = ( %s, %s)",printImprecise(x1),printImprecise(y1));
    printf("\n( x2, y2) = ( %s, %s)",printImprecise(x2),printImprecise(y2));
    printf("\nis d = %s", printImprecise(d));
    return 0;
}
