#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define season( x ) ((x) == 0 ? "Chaos" :\
                    (x) == 1 ? "Discord" :\
                    (x) == 2 ? "Confusion" :\
                    (x) == 3 ? "Bureaucracy" :\
                    "The Aftermath")

#define date( x ) ((x)%73 == 0 ? 73 : (x)%73)

#define leap_year( x ) ((x) % 400 == 0 || (((x) % 4) == 0 && (x) % 100))

char * ddate( int y, int d ){
  int dyear = 1166 + y;
  char * result = malloc( 100 * sizeof( char ) );

  if( leap_year( y ) ){
    if( d == 60 ){
      sprintf( result, "St. Tib's Day, YOLD %d", dyear );
      return result;
    } else if( d >= 60 ){
      -- d;
    }
  }

  sprintf( result, "%s %d, YOLD %d", season( d/73 ), date( d ), dyear );

  return result;
}


int day_of_year( int y, int m, int d ){
  int month_lengths[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

  for( ; m > 1; m -- ){
    d += month_lengths[ m - 2 ];
    if( m == 3 && leap_year( y ) ){
      ++ d;
    }
  }
  return d;
}


int main( int argc, char * argv[] ){
  time_t now;
  struct tm * now_time;
  int year, doy;

  if( argc == 1 ){
    now = time( NULL );
    now_time = localtime( &now );
    year = now_time->tm_year + 1900; doy = now_time->tm_yday + 1;
  } else if( argc == 4 ){
    year = atoi( argv[ 1 ] ); doy = day_of_year( atoi( argv[ 1 ] ), atoi( argv[ 2 ] ), atoi( argv[ 3 ] ) );
  }

  char * result = ddate( year, doy );
  puts( result );
  free( result );

  return 0;
}
