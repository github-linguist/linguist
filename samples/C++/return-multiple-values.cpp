#include <algorithm>
#include <iostream>
#include <tuple>

std::tuple<const int , const int> minmax ( const int * numbers , const int num ) {
   const int *maximum = std::max_element ( numbers , numbers + num ) ;
   const int *minimum = std::min_element ( numbers , numbers + num ) ;
   return std::make_tuple( *maximum , *minimum ) ;
}

int main( ) {
   const int numbers[ ] = { 17 , 88 , 9 , 33 , 4 , 987 , -10 , 2 } ;
   int numbersize = sizeof( numbers ) / sizeof ( int ) ;
   std::tuple<const int , const int> result = minmax( numbers , numbersize ) ;
   std::cout << "The greatest number is " << std::get<0>( result )
      << " , the smallest " << std::get<1>( result ) << " !\n" ;
   return 0 ;
}
