#include <iostream>
#include <fstream>
#include <string>
#include <iterator>

int main( ) {
   std::ifstream infile( "sample.txt" ) ;
   if ( infile ) {
      std::string fileData( ( std::istreambuf_iterator<char> ( infile ) ) ,
	    std::istreambuf_iterator<char> ( ) ) ;
      infile.close( ) ; ;
      return 0 ;
   }
   else {
      std::cout << "file not found!\n" ;
      return 1 ;
   }
}
