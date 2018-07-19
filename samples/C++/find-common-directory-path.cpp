#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

std::string longestPath( const std::vector<std::string> & , char ) ;

int main( ) {
   std::string dirs[ ] = {
      "/home/user1/tmp/coverage/test" ,
      "/home/user1/tmp/covert/operator" ,
      "/home/user1/tmp/coven/members" } ;
   std::vector<std::string> myDirs ( dirs , dirs + 3 ) ;
   std::cout << "The longest common path of the given directories is "
             << longestPath( myDirs , '/' ) << "!\n" ;
   return 0 ;
}

std::string longestPath( const std::vector<std::string> & dirs , char separator ) {
   std::vector<std::string>::const_iterator vsi = dirs.begin( ) ;
   int maxCharactersCommon = vsi->length( ) ;
   std::string compareString = *vsi ;
   for ( vsi = dirs.begin( ) + 1 ; vsi != dirs.end( ) ; vsi++ ) {
      std::pair<std::string::const_iterator , std::string::const_iterator> p =
	 std::mismatch( compareString.begin( ) , compareString.end( ) , vsi->begin( ) ) ;
      if (( p.first - compareString.begin( ) ) < maxCharactersCommon )
	 maxCharactersCommon = p.first - compareString.begin( ) ;
   }
   std::string::size_type found = compareString.rfind( separator , maxCharactersCommon ) ;
   return compareString.substr( 0 , found ) ;
}
