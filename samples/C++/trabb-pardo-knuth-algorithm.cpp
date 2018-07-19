#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <iomanip>

int main( ) {
   std::vector<double> input( 11 ) , results( 11 ) ;
   double number = 0.0 ;
   std::cout << "Please enter 11 numbers!\n" ;
   for ( int i = 0 ; i < input.size( ) ; i++ ) {
      std::cin >> number ;
      input[ i ] = number ;
   }
   std::transform( input.begin( ) , input.end( ) , results.begin( ) ,
	 [ ]( double n )-> double { return sqrt( abs( n ) ) + 5 * pow( n , 3 ) ; } ) ;
   for ( int i = 10 ; i > -1 ; i-- ) {
      std::cout << "f( " << std::setw( 3 ) << input[ i ] << " ) : " ;
      if ( results[ i ] > 400 )
	 std::cout << "too large!" ;
      else
	 std::cout << results[ i ] << " !" ;
      std::cout << std::endl ;
   }
   return 0 ;
}
