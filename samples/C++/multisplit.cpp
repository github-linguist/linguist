#include <iostream>
#include <boost/tokenizer.hpp>
#include <string>

int main( ) {
   std::string str( "a!===b=!=c" ) , output ;
   typedef boost::tokenizer<boost::char_separator<char> > tokenizer ;
   boost::char_separator<char> separator ( "==" , "!=" ) , sep ( "!" )  ;
   tokenizer mytok( str , separator ) ;
   tokenizer::iterator tok_iter = mytok.begin( ) ;
   for ( ; tok_iter != mytok.end( ) ; ++tok_iter )
      output.append( *tok_iter ) ;
   tokenizer nexttok ( output , sep ) ;
   for ( tok_iter = nexttok.begin( ) ; tok_iter != nexttok.end( ) ;
	 ++tok_iter )
      std::cout << *tok_iter << " " ;
   std::cout << '\n' ;
   return 0 ;
}
