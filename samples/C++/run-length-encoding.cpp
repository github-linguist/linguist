#include <iostream>
#include <string>
#include <sstream>
#include <boost/regex.hpp>
#include <cstdlib>

std::string encode ( const std::string & ) ;
std::string decode ( const std::string & ) ;

int main( ) {
   std::string to_encode ( "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW" ) ;
   std::cout << to_encode << " encoded:" << std::endl ;
   std::string encoded ( encode ( to_encode ) ) ;
   std::cout << encoded << std::endl ;
   std::string decoded ( decode( encoded ) ) ;
   std::cout << "Decoded again:\n" ;
   std::cout << decoded << std::endl ;
   if ( to_encode == decoded )
      std::cout << "It must have worked!\n" ;
   return 0 ;
}

std::string encode( const std::string & to_encode ) {
   std::string::size_type found = 0 , nextfound = 0 ;
   std::ostringstream oss ;
   nextfound = to_encode.find_first_not_of( to_encode[ found ] , found ) ;
   while ( nextfound != std::string::npos ) {
      oss << nextfound - found ;
      oss << to_encode[ found ] ;
      found = nextfound ;
      nextfound = to_encode.find_first_not_of( to_encode[ found ] , found ) ;
   }
   //since we must not discard the last characters we add them at the end of the string
   std::string rest ( to_encode.substr( found ) ) ;//last run of characters starts at position found
   oss << rest.length( ) << to_encode[ found ] ;
   return oss.str( ) ;
}

std::string decode ( const std::string & to_decode ) {
   boost::regex e ( "(\\d+)(\\w)" ) ;
   boost::match_results<std::string::const_iterator> matches ;
   std::ostringstream oss ;
   std::string::const_iterator start = to_decode.begin( ) , end = to_decode.end( ) ;
   while ( boost::regex_search ( start , end , matches , e ) ) {
      std::string numberstring ( matches[ 1 ].first , matches[ 1 ].second ) ;
      int number = atoi( numberstring.c_str( ) ) ;
      std::string character ( matches[ 2 ].first , matches[ 2 ].second ) ;
      for ( int i = 0 ; i < number ; i++ )
	 oss << character ;
      start = matches[ 2 ].second ;
   }
   return oss.str( ) ;
}
