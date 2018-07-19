#include <stdio.h>
#include <unistd.h>

int main()
{
  unsigned int seconds;
  scanf("%u", &seconds);
  printf("Sleeping...\n");
  sleep(seconds);
  printf("Awake!\n");
  return 0;
}
