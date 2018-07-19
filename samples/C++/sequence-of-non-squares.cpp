#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <boost/bind.hpp>
#include <iterator>

double nextNumber( double number ) {
   return number + floor( 0.5 + sqrt( number ) ) ;
}

int main( ) {
   std::vector<double> non_squares ;
   typedef std::vector<double>::iterator SVI ;
   non_squares.reserve( 1000000 ) ;
   //create a vector with a million sequence numbers
   for ( double i = 1.0 ; i < 100001.0 ; i += 1 )
      non_squares.push_back( nextNumber( i ) ) ;
   //copy the first numbers to standard out
   std::copy( non_squares.begin( ) , non_squares.begin( ) + 22 ,
	 std::ostream_iterator<double>(std::cout, " " ) ) ;
   std::cout << '\n' ;
   //find if floor of square root equals square root( i. e. it's a square number )
   SVI found = std::find_if ( non_squares.begin( ) , non_squares.end( ) ,
	 boost::bind( &floor, boost::bind( &sqrt, _1 ) ) == boost::bind( &sqrt, _1 ) ) ;
   if ( found != non_squares.end( ) ) {
      std::cout << "Found a square number in the sequence!\n" ;
      std::cout << "It is " << *found << " !\n" ;
   }
   else {
      std::cout << "Up to 1000000, found no square number in the sequence!\n" ;
   }
   return 0 ;
}
