#include <vector>
#include <iostream>
#include <numeric>
#include <cmath>
#include <algorithm>

double toInverse ( int i ) {
   return  1.0 / i  ;
}

int main( ) {
   std::vector<int> numbers ;
   for ( int i = 1 ; i < 11 ; i++ )
      numbers.push_back( i ) ;
   double arithmetic_mean = std::accumulate( numbers.begin( ) , numbers.end( ) , 0 ) / 10.0 ;
   double geometric_mean =
      pow( std::accumulate( numbers.begin( ) , numbers.end( ) , 1 , std::multiplies<int>( ) ), 0.1 ) ;
   std::vector<double> inverses ;
   inverses.resize( numbers.size( ) ) ;
   std::transform( numbers.begin( ) , numbers.end( ) , inverses.begin( ) , toInverse ) ;
   double harmonic_mean = 10 / std::accumulate( inverses.begin( ) , inverses.end( ) , 0.0 ); //initial value of accumulate must be a double!
   std::cout << "The arithmetic mean is " << arithmetic_mean << " , the geometric mean "
      << geometric_mean << " and the harmonic mean " << harmonic_mean << " !\n" ;
   return 0 ;
}
