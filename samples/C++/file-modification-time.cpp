#include <boost/filesystem/operations.hpp>
#include <ctime>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   if ( argc != 2 ) {
      std::cerr << "Error! Syntax: moditime <filename>!\n" ;
      return 1 ;
   }
   boost::filesystem::path p( argv[ 1 ] ) ;
   if ( boost::filesystem::exists( p ) ) {
      std::time_t t = boost::filesystem::last_write_time( p ) ;
      std::cout << "On " << std::ctime( &t ) << " the file " << argv[ 1 ]
	 << " was modified the last time!\n" ;
      std::cout << "Setting the modification time to now:\n" ;
      std::time_t n = std::time( 0 ) ;
      boost::filesystem::last_write_time( p , n ) ;
      t = boost::filesystem::last_write_time( p ) ;
      std::cout << "Now the modification time is " << std::ctime( &t ) << std::endl ;
      return 0 ;
   } else {
      std::cout << "Could not find file " << argv[ 1 ] << '\n' ;
      return 2 ;
   }
}
