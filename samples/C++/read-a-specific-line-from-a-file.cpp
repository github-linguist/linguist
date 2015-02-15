#include <string>
#include <fstream>
#include <iostream>

int main( ) {
   std::cout << "Which file do you want to look at ?\n" ;
   std::string input ;
   std::getline( std::cin , input ) ;
   std::ifstream infile( input.c_str( ) , std::ios::in ) ;
   std::string file( input ) ;
   std::cout << "Which file line do you want to see ? ( Give a number > 0 ) ?\n" ;
   std::getline( std::cin , input ) ;
   int linenumber = std::stoi( input ) ;
   int lines_read = 0 ;
   std::string line ;
   if ( infile.is_open( ) ) {
      while ( infile ) {
	 getline( infile , line ) ;
	 lines_read++ ;
	 if ( lines_read == linenumber ) {
	    std::cout << line << std::endl ;
	    break ;
	 }
      }
      infile.close( ) ;
      if ( lines_read < linenumber )
	 std::cout << "No " << linenumber << " lines in " << file << " !\n" ;
      return 0 ;
   }
   else {
      std::cerr << "Could not find file " << file << " !\n" ;
      return 1 ;
   }
}
