#include <string>
#include <iostream>

int main( ) {
   std::string word( "Premier League" ) ;
   std::cout << "Without first letter: " << word.substr( 1 ) << " !\n" ;
   std::cout << "Without last letter: " << word.substr( 0 , word.length( ) - 1 ) << " !\n" ;
   std::cout << "Without first and last letter: " << word.substr( 1 , word.length( ) - 2 ) << " !\n" ;
   return 0 ;
}
