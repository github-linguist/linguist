#include <stdio.h>
#include <stdbool.h>

bool a(bool in)
{
  printf("I am a\n");
  return in;
}

bool b(bool in)
{
  printf("I am b\n");
  return in;
}

#define TEST(X,Y,O)						\
  do {								\
    x = a(X) O b(Y);						\
    printf(#X " " #O " " #Y " = %s\n\n", x ? "true" : "false");	\
  } while(false);

int main()
{
  bool x;

  TEST(false, true, &&); // b is not evaluated
  TEST(true, false, ||); // b is not evaluated
  TEST(true, false, &&); // b is evaluated
  TEST(false, false, ||); // b is evaluated

  return 0;
}
