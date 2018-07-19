#include <string>
#include <iostream>

int main( ) {
   std::string original( "Mary had a X lamb." ) , toBeReplaced( "X" ) ,
      replacement ( "little" ) ;
   std::string newString = original.replace( original.find( "X" ) ,
	 toBeReplaced.length( ) , replacement ) ;
   std::cout << "String after replacement: " << newString << " \n" ;
   return 0 ;
}
