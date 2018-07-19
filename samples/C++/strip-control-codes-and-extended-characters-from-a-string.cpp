#include <string>
#include <iostream>
#include <algorithm>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/casts.hpp>
#include <ctime>
#include <cstdlib>
using namespace boost::lambda ;

struct MyRandomizer {
   char operator( )( ) {
      return static_cast<char>( rand( ) % 256 ) ;
   }
} ;

std::string deleteControls ( std::string startstring ) {
   std::string noControls( "                                        " ) ;//creating space for
   //the standard algorithm remove_copy_if
   std::remove_copy_if( startstring.begin( ) , startstring.end( ) , noControls.begin( ) ,
	 ll_static_cast<int>( _1 ) < 32 && ll_static_cast<int>( _1 ) == 127 ) ;
   return noControls ;
}

std::string deleteExtended( std::string startstring ) {
   std::string noExtended ( "                                        " ) ;//same as above
   std::remove_copy_if( startstring.begin( ) , startstring.end( ) , noExtended.begin( ) ,
	 ll_static_cast<int>( _1 ) > 127 || ll_static_cast<int>( _1 ) < 32 ) ;
   return noExtended ;
}

int main( ) {
   std::string my_extended_string ;
   for ( int i = 0 ; i < 40 ; i++ ) //we want the extended string to be 40 characters long
      my_extended_string.append( " " ) ;
   srand( time( 0 ) ) ;
   std::generate_n( my_extended_string.begin( ) , 40 , MyRandomizer( ) ) ;
   std::string no_controls( deleteControls( my_extended_string ) ) ;
   std::string no_extended ( deleteExtended( my_extended_string ) ) ;
   std::cout << "string with all characters: " << my_extended_string << std::endl ;
   std::cout << "string without control characters: " << no_controls << std::endl ;
   std::cout << "string without extended characters: " << no_extended << std::endl ;
   return 0 ;
}
