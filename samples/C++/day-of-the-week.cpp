#include <boost/date_time/gregorian/gregorian.hpp>
#include <iostream>

int main( ) {
   using namespace boost::gregorian ;

   std::cout
      << "Yuletide holidays must be allowed in the following years:\n" ;
   for ( int i = 2008 ; i < 2121 ; i++ ) {
      greg_year gy = i ;
      date d  ( gy, Dec , 25 ) ;
      if ( d.day_of_week( ) == Sunday ) {
	 std::cout << i << std::endl ;
      }
   }
   std::cout << "\n" ;
   return 0 ;
}
