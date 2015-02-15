#include <vector>
#include <iostream>
#include <numeric>
#include <iterator>
#include <memory>
#include <string>
#include <algorithm>
#include <iomanip>

std::vector<int> nacci ( const std::vector<int> & start , int arity ) {
   std::vector<int> result ( start ) ;
   int sumstart = 1 ;//summing starts at vector's begin + sumstart as
                     //soon as the vector is longer than arity
   while ( result.size( ) < 15 ) { //we print out the first 15 numbers
      if ( result.size( ) <= arity )
	 result.push_back( std::accumulate( result.begin( ) ,
		  result.begin( ) + result.size( ) , 0 ) ) ;
      else {
	 result.push_back( std::accumulate ( result.begin( ) +
	  sumstart , result.begin( ) + sumstart + arity  , 0 )) ;
	 sumstart++ ;
      }
   }
   return std::move ( result ) ;
}

int main( ) {
   std::vector<std::string> naccinames {"fibo" , "tribo" ,
      "tetra" , "penta" , "hexa" , "hepta" , "octo" , "nona" , "deca" } ;
   const std::vector<int> fibo { 1 , 1 } , lucas { 2 , 1 } ;
   for ( int i = 2 ; i < 11 ; i++ ) {
      std::vector<int> numberrow = nacci ( fibo , i ) ;
      std::cout << std::left << std::setw( 10 ) <<
	 naccinames[ i - 2 ].append( "nacci" ) <<
	 std::setw( 2 ) << " : " ;
      std::copy ( numberrow.begin( ) , numberrow.end( ) ,
	    std::ostream_iterator<int>( std::cout , " " ) ) ;
      std::cout << "...\n" ;
      numberrow = nacci ( lucas , i ) ;
      std::cout << "Lucas-" << i ;
      if ( i < 10 )               //for formatting purposes
	 std::cout << "    : " ;
      else
	 std::cout << "   : " ;
      std::copy ( numberrow.begin( ) , numberrow.end( ) ,
	    std::ostream_iterator<int>( std::cout , " " ) ) ;
      std::cout << "...\n" ;
   }
   return 0 ;
}
