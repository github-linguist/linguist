#include <string>
#include "Poco/URI.h"
#include <iostream>

int main( ) {
   std::string encoded( "http%3A%2F%2Ffoo%20bar%2F" ) ;
   std::string decoded ;
   Poco::URI::decode ( encoded , decoded ) ;
   std::cout << encoded << " is decoded: " << decoded << " !" << std::endl ;
   return 0 ;
}
