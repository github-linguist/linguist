#include <stdio.h>
#include <stdbool.h>

void halve(int *x) { *x >>= 1; }
void doublit(int *x)  { *x <<= 1; }
bool iseven(const int x) { return (x & 1) ==  0; }

int ethiopian(int plier,
	      int plicand, const bool tutor)
{
  int result=0;

  if (tutor)
    printf("ethiopian multiplication of %d by %d\n", plier, plicand);

  while(plier >= 1) {
    if ( iseven(plier) ) {
      if (tutor) printf("%4d %6d struck\n", plier, plicand);
    } else {
      if (tutor) printf("%4d %6d kept\n", plier, plicand);
      result += plicand;
    }
    halve(&plier); doublit(&plicand);
  }
  return result;
}

int main()
{
  printf("%d\n", ethiopian(17, 34, true));
  return 0;
}
