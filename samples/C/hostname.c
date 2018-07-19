#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <unistd.h>

int main(void)
{
 char name[_POSIX_HOST_NAME_MAX + 1];
 return gethostname(name, sizeof name) == -1 || printf("%s\n", name) < 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
