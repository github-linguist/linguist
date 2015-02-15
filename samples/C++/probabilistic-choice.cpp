#include <cstdlib>
#include <iostream>
#include <vector>
#include <utility>
#include <algorithm>
#include <ctime>
#include <iomanip>

int main( ) {
   typedef std::vector<std::pair<std::string, double> >::const_iterator SPI ;
   typedef std::vector<std::pair<std::string , double> > ProbType ;
   ProbType probabilities ;
   probabilities.push_back( std::make_pair( "aleph" , 1/5.0 ) ) ;
   probabilities.push_back( std::make_pair( "beth" , 1/6.0 ) ) ;
   probabilities.push_back( std::make_pair( "gimel" , 1/7.0 ) ) ;
   probabilities.push_back( std::make_pair( "daleth" , 1/8.0 ) ) ;
   probabilities.push_back( std::make_pair( "he" , 1/9.0 ) ) ;
   probabilities.push_back( std::make_pair( "waw" , 1/10.0 ) ) ;
   probabilities.push_back( std::make_pair( "zayin" , 1/11.0 ) ) ;
   probabilities.push_back( std::make_pair( "heth" , 1759/27720.0 ) ) ;
   std::vector<std::string> generated ; //for the strings that are generatod
   std::vector<int> decider ; //holds the numbers that determine the choice of letters
   for ( int i = 0 ; i < probabilities.size( ) ; i++ ) {
      if ( i == 0 ) {
	 decider.push_back( 27720 * (probabilities[ i ].second) ) ;
      }
      else {
	 int number = 0 ;
	 for ( int j = 0 ; j < i ; j++ ) {
	    number +=  27720 * ( probabilities[ j ].second ) ;
	 }
	 number += 27720 * probabilities[ i ].second ;
	 decider.push_back( number ) ;
      }
   }
   srand( time( 0 ) ) ;
   for ( int i = 0 ; i < 1000000 ; i++ ) {
      int randnumber = rand( ) % 27721 ;
      int j = 0 ;
      while ( randnumber > decider[ j ] )
	 j++ ;
      generated.push_back( ( probabilities[ j ]).first ) ;
   }
   std::cout << "letter  frequency attained   frequency expected\n" ;
   for ( SPI i = probabilities.begin( ) ; i != probabilities.end( ) ; i++ ) {
      std::cout << std::left << std::setw( 8 ) << i->first ;
      int found = std::count ( generated.begin( ) , generated.end( ) , i->first ) ;
      std::cout << std::left << std::setw( 21 ) << found / 1000000.0 ;
      std::cout << std::left << std::setw( 17 ) << i->second << '\n' ;
   }
   return 0 ;
}
