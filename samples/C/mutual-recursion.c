#include <stdio.h>
#include <stdlib.h>

/* let us declare our functions; indeed here we need
   really only M declaration, so that F can "see" it
   and the compiler won't complain with a warning */
int F(const int n);
int M(const int n);

int F(const int n)
{
  return (n == 0) ? 1 : n - M(F(n - 1));
}

int M(const int n)
{
  return (n == 0) ? 0 : n - F(M(n - 1));
}

int main(void)
{
  int i;
  for (i = 0; i < 20; i++)
    printf("%2d ", F(i));
  printf("\n");
  for (i = 0; i < 20; i++)
    printf("%2d ", M(i));
  printf("\n");
  return EXIT_SUCCESS;
}
