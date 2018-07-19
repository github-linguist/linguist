#include <math.h>
#include <stdio.h>

int main() {
  double pi = 4 * atan(1);
  /*Pi / 4 is 45 degrees. All answers should be the same.*/
  double radians = pi / 4;
  double degrees = 45.0;
  double temp;
  /*sine*/
  printf("%f %f\n", sin(radians), sin(degrees * pi / 180));
  /*cosine*/
  printf("%f %f\n", cos(radians), cos(degrees * pi / 180));
  /*tangent*/
  printf("%f %f\n", tan(radians), tan(degrees * pi / 180));
  /*arcsine*/
  temp = asin(sin(radians));
  printf("%f %f\n", temp, temp * 180 / pi);
  /*arccosine*/
  temp = acos(cos(radians));
  printf("%f %f\n", temp, temp * 180 / pi);
  /*arctangent*/
  temp = atan(tan(radians));
  printf("%f %f\n", temp, temp * 180 / pi);

  return 0;
}
