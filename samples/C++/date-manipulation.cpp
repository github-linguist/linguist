#include <string>
#include <iostream>
#include <boost/date_time/local_time/local_time.hpp>
#include <sstream>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <vector>
#include <boost/algorithm/string.hpp>
#include <cstdlib>
#include <locale>


int main( ) {
   std::string datestring ("March 7 2009 7:30pm EST" ) ;
   //we must first parse the date string into a date , a time and a time
   //zone part , to take account of present restrictions in the input facets
   //of the Boost::DateTime library used for this example
   std::vector<std::string> elements ;
   //parsing the date string
   boost::split( elements , datestring , boost::is_any_of( " " ) ) ;
   std::string datepart = elements[ 0 ] + " " + "0" + elements[ 1 ] + " " +
      elements[ 2 ] ; //we must add 0 to avoid trouble with the boost::date_input format strings
   std::string timepart = elements[ 3 ] ;
   std::string timezone = elements[ 4 ] ;
   const char meridians[ ] = { 'a' , 'p' } ;
   //we have to find out if the time is am or pm, to change the hours appropriately
   std::string::size_type found = timepart.find_first_of( meridians, 0 ) ;
   std::string twelve_hour ( timepart.substr( found , 1 ) ) ;
   timepart = timepart.substr( 0 , found ) ; //we chop off am or pm
   elements.clear( ) ;
   boost::split( elements , timepart , boost::is_any_of ( ":" ) ) ;
   long hour = std::atol( (elements.begin( ))->c_str( ) ) ;// hours in the string
   if ( twelve_hour == "p" ) //it's post meridian, we're converting to 24-hour-clock
      hour += 12 ;
   long minute = std::atol( ( elements.begin( ) + 1)->c_str( ) ) ;
   boost::local_time::tz_database tz_db ;
   tz_db.load_from_file( "/home/ulrich/internetpages/date_time_zonespec.csv" ) ;
   //according to the time zone database, this corresponds to one possible EST time zone
   boost::local_time::time_zone_ptr dyc = tz_db.time_zone_from_region( "America/New_York" ) ;
   //this is the string input format to initialize the date field
   boost::gregorian::date_input_facet *f =
      new boost::gregorian::date_input_facet( "%B %d %Y"  ) ;
   std::stringstream ss ;
   ss << datepart ;
   ss.imbue( std::locale( std::locale::classic( ) , f ) ) ;
   boost::gregorian::date d ;
   ss >> d ;
   boost::posix_time::time_duration td (  hour , minute , 0  ) ;
   //that's how we initialize the New York local time , by using date and adding
   //time duration with values coming from parsed date input string
   boost::local_time::local_date_time lt ( d , td ,  dyc ,
	 boost::local_time::local_date_time::NOT_DATE_TIME_ON_ERROR ) ;
   std::cout << "local time: " << lt << '\n' ;
   ss.str( "" ) ;
   ss << lt ;
   //we have to add 12 hours, so a new time duration object is created
   boost::posix_time::time_duration td2 (12 , 0 , 0 , 0 ) ;
   boost::local_time::local_date_time ltlater = lt + td2 ; //local time 12 hours later
   boost::gregorian::date_facet *f2 =
      new boost::gregorian::date_facet( "%B %d %Y , %R %Z" ) ;
   std::cout.imbue( std::locale( std::locale::classic( ) , f2 ) ) ;
   std::cout << "12 hours after " << ss.str( )  << " it is " << ltlater << " !\n" ;
   //what's New York time in the Berlin time zone ?
   boost::local_time::time_zone_ptr bt = tz_db.time_zone_from_region( "Europe/Berlin" ) ;
   std::cout.imbue( std::locale( "de_DE.UTF-8" ) ) ; //choose the output forman appropriate for the time zone
   std::cout << "This corresponds to " << ltlater.local_time_in( bt ) << " in Berlin!\n" ;
   return 0 ;
}
