#include <iostream>

int main( ) {
   int hofstadters[100000] ;
   hofstadters[ 0 ] = 1 ;
   hofstadters[ 1 ] = 1 ;
   for ( int i = 3 ; i < 100000 ; i++ )
      hofstadters[ i - 1 ] = hofstadters[ i - 1 - hofstadters[ i - 1 - 1 ]] +
	 hofstadters[ i - 1 - hofstadters[ i - 2 - 1 ]] ;
   std::cout << "The first 10 numbers are:\n" ;
   for ( int i = 0 ; i < 10 ; i++ )
      std::cout << hofstadters[ i ] << std::endl ;
   std::cout << "The 1000'th term is " << hofstadters[ 999 ] << " !" << std::endl ;
   int less_than_preceding = 0 ;
   for ( int i = 0 ; i < 99999 ; i++ ) {
      if ( hofstadters[ i + 1 ] < hofstadters[ i ] )
	 less_than_preceding++ ;
   }
   std::cout << less_than_preceding << " times a number was preceded by a greater number!\n" ;
   return 0 ;
}
