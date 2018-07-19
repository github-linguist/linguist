#include <stdio.h>

int main()
{
  int i;

  for(i=1; i <= 33; i++)
    printf("%6d %6x %6o\n", i, i, i);

  return 0;
}
