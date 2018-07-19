#include <string>
#include <map>
#include <iostream>
#include <algorithm>
#include <cmath>

double log2( double number ) {
   return log( number ) / log( 2 ) ;
}

int main( int argc , char *argv[ ] ) {
   std::string teststring( argv[ 1 ] ) ;
   std::map<char , int> frequencies ;
   for ( char c : teststring )
     frequencies[ c ] ++ ;
   int numlen = teststring.length( ) ;
   double infocontent = 0 ;
   for ( std::pair<char , int> p : frequencies ) {
      double freq = static_cast<double>( p.second ) / numlen ;
      infocontent += freq * log2( freq ) ;
   }
   infocontent *= -1 ;
   std::cout << "The information content of " << teststring
      << " is " << infocontent << " !\n" ;
   return 0 ;
}
