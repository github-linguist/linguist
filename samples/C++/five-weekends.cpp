#include <vector>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <algorithm>
#include <iostream>
#include <iterator>
using namespace boost::gregorian ;

void print( const date &d ) {
   std::cout << d.year( ) << "-" << d.month( ) << "\n" ;
}

int main( ) {
   greg_month longmonths[ ] = {Jan, Mar , May , Jul ,
      Aug , Oct , Dec } ;
   int monthssize = sizeof ( longmonths ) / sizeof (greg_month ) ;
   typedef std::vector<date> DateVector ;
   DateVector weekendmonster ;
   std::vector<unsigned short> years_without_5we_months ;
   for ( unsigned short i = 1900 ; i < 2101 ; i++ ) {
      bool months_found = false ; //does a given year have 5 weekend months ?
      for ( int j = 0 ; j < monthssize ; j++ ) {
	 date d ( i , longmonths[ j ] , 1 ) ;
	 if ( d.day_of_week( ) == Friday ) {  //for the month to have 5 weekends
	    weekendmonster.push_back( d ) ;
	    if ( months_found == false )
	       months_found = true ;
         }
      }
      if ( months_found == false ) {
	 years_without_5we_months.push_back( i ) ;
      }
   }
   std::cout << "Between 1900 and 2100 , there are " << weekendmonster.size( )
      << " months with 5 complete weekends!\n" ;
   std::cout << "Months with 5 complete weekends are:\n" ;
   std::for_each( weekendmonster.begin( ) , weekendmonster.end( ) , print ) ;
   std::cout <<  years_without_5we_months.size( ) << " years had no months with 5 complete weekends!\n" ;
   std::cout << "These are:\n" ;
   std::copy( years_without_5we_months.begin( ) , years_without_5we_months.end( ) ,
	 std::ostream_iterator<unsigned short>( std::cout , "\n" ) ) ;
   std::cout << std::endl ;
   return 0 ;
}
