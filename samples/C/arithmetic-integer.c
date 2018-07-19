#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int a, b;
  if (argc < 3) exit(1);
  b = atoi(argv[--argc]);
  if (b == 0) exit(2);
  a = atoi(argv[--argc]);
  printf("a+b = %d\n", a+b);
  printf("a-b = %d\n", a-b);
  printf("a*b = %d\n", a*b);
  printf("a/b = %d\n", a/b); /* truncates towards 0 (in C99) */
  printf("a%%b = %d\n", a%b); /* same sign as first operand (in C99) */
  return 0;
}
