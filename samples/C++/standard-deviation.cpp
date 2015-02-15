#include <algorithm>
#include <iostream>
#include <iterator>
#include <cmath>
#include <vector>
#include <iterator>
#include <numeric>

template <typename Iterator>
double standard_dev( Iterator begin , Iterator end ) {
   double mean = std::accumulate( begin , end , 0 ) / std::distance( begin , end ) ;
   std::vector<double> squares ;
   for( Iterator vdi = begin ; vdi != end ; vdi++ )
      squares.push_back( std::pow( *vdi - mean , 2 ) ) ;
   return std::sqrt( std::accumulate( squares.begin( ) , squares.end( ) , 0 ) / squares.size( ) ) ;
}

int main( ) {
   double demoset[] = { 2 , 4 , 4 , 4 , 5 , 5 , 7 , 9 } ;
   int demosize = sizeof demoset / sizeof *demoset ;
   std::cout << "The standard deviation of\n" ;
   std::copy( demoset , demoset + demosize , std::ostream_iterator<double>( std::cout, " " ) ) ;
   std::cout << "\nis " << standard_dev( demoset , demoset + demosize ) << " !\n" ;
   return 0 ;
}
