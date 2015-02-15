#include <stdio.h>
#include <math.h>

#define PICKVALUE(TXT, VM) do {			\
    printf("%s: ", TXT);			\
    scanf("%lf", &VM);				\
  } while(0);

#if !defined(M_PI)
#define M_PI 3.14159265358979323846
#endif

#define DR(X) ((X)*M_PI/180.0)
#define RD(X) ((X)*180.0/M_PI)

int main()
{
  double lat, slat, lng, ref;
  int h;

  PICKVALUE("Enter latitude", lat);
  PICKVALUE("Enter longitude", lng);
  PICKVALUE("Enter legal meridian", ref);
  printf("\n");

  slat = sin(DR(lat));
  printf("sine of latitude: %.3f\n", slat);
  printf("diff longitude: %.3f\n\n", lng - ref);

  printf("Hour, sun hour angle, dial hour line angle from 6am to 6pm\n");

  for(h = -6; h <= 6; h++)
  {
    double hla, hra;
    hra = 15.0*h;
    hra = hra - lng + ref;
    hla = RD(atan(slat * tan(DR(hra))));
    printf("HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n",
	   h, hra, hla);
  }

  return 0;
}
