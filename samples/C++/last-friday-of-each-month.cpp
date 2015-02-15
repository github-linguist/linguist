#include <boost/date_time/gregorian/gregorian.hpp>
#include <iostream>
#include <cstdlib>

int main( int argc , char* argv[ ] ) {
   using namespace boost::gregorian ;

   greg_month months[ ] = { Jan , Feb , Mar , Apr , May , Jun , Jul ,
      Aug , Sep , Oct , Nov , Dec } ;
   greg_year gy = atoi( argv[ 1 ] ) ;
   for ( int i = 0 ; i < 12 ; i++ ) {
      last_day_of_the_week_in_month lwdm ( Friday , months[ i ] ) ;
      date d = lwdm.get_date( gy ) ;
      std::cout << d << std::endl ;
   }
   return 0 ;
}
