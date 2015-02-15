#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main( int argc, char ** argv ){
  const char * str_a = "knight";
  const char * str_b = "socks";
  const char * str_c = "brooms";

  char * new_a = malloc( strlen( str_a ) - 1 );
  char * new_b = malloc( strlen( str_b ) - 1 );
  char * new_c = malloc( strlen( str_c ) - 2 );

  strcpy( new_a, str_a + 1 );
  strncpy( new_b, str_b, strlen( str_b ) - 1 );
  strncpy( new_c, str_c + 1, strlen( str_c ) - 2 );

  printf( "%s\n%s\n%s\n", new_a, new_b, new_c );

  free( new_a );
  free( new_b );
  free( new_c );

  return 0;
}
