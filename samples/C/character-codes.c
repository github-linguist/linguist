#include <stdio.h>

int main() {
  printf("%d\n", 'a'); /* prints "97" */
  printf("%c\n", 97); /* prints "a"; we don't have to cast because printf is type agnostic */
  return 0;
}
