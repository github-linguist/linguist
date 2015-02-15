#include <fstream>
#include <iterator>
#include <boost/regex.hpp>
#include <string>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   boost::regex to_be_replaced( "Goodbye London\\s*!" ) ;
   std::string replacement( "Hello New York!" ) ;
   for ( int i = 1 ; i < argc ; i++ ) {
      std::ifstream infile ( argv[ i ] ) ;
      if ( infile ) {
	 std::string filetext( (std::istreambuf_iterator<char>( infile )) ,
	       std::istreambuf_iterator<char>( ) ) ;
	 std::string changed ( boost::regex_replace( filetext , to_be_replaced , replacement )) ;
	 infile.close( ) ;
	 std::ofstream outfile( argv[ i ] , std::ios_base::out | std::ios_base::trunc ) ;
	 if ( outfile.is_open( ) ) {
	    outfile << changed ;
	    outfile.close( ) ;
	 }
      }
      else
	 std::cout << "Can't find file " << argv[ i ] << " !\n" ;
   }
   return 0 ;
}
