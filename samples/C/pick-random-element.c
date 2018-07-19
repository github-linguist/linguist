#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
  char array[] = { 'a', 'b', 'c' };

  srand( time( 0 ) );

  printf( "%c\n", array[ rand() % 3 ] );

  return 0;
}
