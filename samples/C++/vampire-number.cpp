#include <vector>
#include <utility>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <cmath>

bool isVampireNumber( long number, std::vector<std::pair<long, long> > & solution ) {
   std::ostringstream numberstream ;
   numberstream << number ;
   std::string numberstring( numberstream.str( ) ) ;
   std::sort ( numberstring.begin( ) , numberstring.end( ) ) ;
   int fanglength = numberstring.length( ) / 2 ;
   long start = static_cast<long>( std::pow( 10 , fanglength - 1 ) ) ;
   long end = start * 10 ;
   for ( long i = start ; i < ( end - start ) / 2 ; i++ ) {
      if ( number % i == 0 ) {
	 long quotient = number / i ;
	 if ( ( i % 10 == 0 ) && ( quotient % 10 == 0 ) )
	    return false ;
	 numberstream.str( "" ) ; //clear the number stream
	 numberstream << i << quotient ;
	 std::string divisorstring ( numberstream.str( ) ) ;
         std::sort ( divisorstring.begin( ) , divisorstring.end( ) ) ;
	 if ( divisorstring == numberstring ) {
	    std::pair<long , long> divisors = std::make_pair( i, quotient ) ;
	    solution.push_back( divisors ) ;
	 }
      }
   }
   return !solution.empty( ) ;
}

void printOut( const std::pair<long, long> & solution ) {
   std::cout << "[ " << solution.first << " , " << solution.second << " ]" ;
}

int main( ) {
   int vampireNumbersFound = 0 ;
   std::vector<std::pair<long , long> > solutions ;
   double i = 1.0 ;
   while ( vampireNumbersFound < 25 ) {
      long start = static_cast<long>( std::pow( 10 , i ) ) ;
      long end = start * 10 ;
      for ( long num = start ; num < end ; num++ ) {
	 if ( isVampireNumber( num , solutions ) ) {
	    std::cout << vampireNumbersFound << " :" << num << " is a vampire number! These are the fangs:\n" ;
	    std::for_each( solutions.begin( ) , solutions.end( ) , printOut ) ;
	    std::cout << "\n_______________" << std::endl ;
	    solutions.clear( ) ;
	    vampireNumbersFound++ ;
	    if ( vampireNumbersFound == 25 )
	       break ;
	 }
      }
      i += 2.0 ;
   }
   std::vector<long> testnumbers ;
   testnumbers.push_back( 16758243290880 ) ;
   testnumbers.push_back( 2495901734865 ) ;
   testnumbers.push_back( 14593825548650 ) ;
   for ( std::vector<long>::const_iterator svl = testnumbers.begin( ) ;
	 svl != testnumbers.end( ) ; svl++ ) {
      if ( isVampireNumber( *svl , solutions ) ) {
	 std::cout << *svl << " is a vampire number! The fangs:\n" ;
	 std::for_each( solutions.begin( ) , solutions.end( ) , printOut ) ;
	 std::cout << std::endl ;
	 solutions.clear( ) ;
      } else {
	 std::cout << *svl << " is not a vampire number!" << std::endl ;
      }
   }
   return 0 ;
}
