#include <iostream>
#include <sstream>
#include <algorithm>
#include <vector>
#include <string>

std::string findLargestConcat ( std::vector< int > & mynumbers ) {
   std::vector<std::string> concatnumbers ;
   std::sort ( mynumbers.begin( ) , mynumbers.end( ) ) ;
   do {
      std::ostringstream numberstream ;
      for ( int i : mynumbers )
	 numberstream << i ;
      concatnumbers.push_back( numberstream.str( ) ) ;
   } while ( std::next_permutation( mynumbers.begin( ) ,
	    mynumbers.end( ) )) ;
   return *( std::max_element( concatnumbers.begin( ) ,
	 concatnumbers.end( ) ) ) ;
}

int main( ) {
   std::vector<int> mynumbers = { 98, 76 , 45 , 34, 9 , 4 , 3 , 1 } ;
   std::vector<int> othernumbers = { 54 , 546 , 548 , 60 } ;
   std::cout << "The largest concatenated int is " <<
      findLargestConcat( mynumbers ) << " !\n" ;
   std::cout << "And here it is " << findLargestConcat( othernumbers )
      << " !\n" ;
   return 0 ;
}
