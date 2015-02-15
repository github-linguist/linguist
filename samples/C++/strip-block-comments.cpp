#include <string>
#include <iostream>
#include <iterator>
#include <fstream>
#include <boost/regex.hpp>

int main( ) {
    std::ifstream codeFile( "samplecode.txt" ) ;
    if ( codeFile ) {
       boost::regex commentre( "/\\*.*?\\*/" ) ;//comment start and end, and as few characters in between as possible
       std::string my_erase( "" ) ;             //erase them
       std::string stripped ;
       std::string code( (std::istreambuf_iterator<char>( codeFile ) ) ,
	     std::istreambuf_iterator<char>( ) ) ;
       codeFile.close( ) ;
       stripped = boost::regex_replace( code , commentre , my_erase ) ;
       std::cout << "Code unstripped:\n" << stripped << std::endl ;
       return 0 ;
    }
    else {
       std::cout << "Could not find code file!" << std::endl ;
       return 1 ;
    }
}
