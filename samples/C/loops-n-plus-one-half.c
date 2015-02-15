#include <stdio.h>

int main()
{
  int i;
  for (i = 1; i <= 10; i++) {
    printf("%d", i);
    printf(i == 10 ? "\n" : ", ");
  }
  return 0;
}
