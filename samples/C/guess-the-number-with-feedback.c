#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define lower_limit 0
#define upper_limit 100

int main(){
  int number, guess;

  srand( time( 0 ) );
  number = lower_limit + rand() % (upper_limit - lower_limit + 1);

  printf( "Guess the number between %d and %d: ", lower_limit, upper_limit );

  while( scanf( "%d", &guess ) == 1 ){
    if( number == guess ){
      printf( "You guessed correctly!\n" );
      break;
    }
    printf( "Your guess was too %s.\nTry again: ", number < guess ? "high" : "low" );
  }

  return 0;
}
