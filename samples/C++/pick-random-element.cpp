#include <iostream>
#include <random>
#include <vector>

int main( ) {
   std::vector<int> numbers { 11 , 88 , -5 , 13 , 4 , 121 , 77 , 2 } ;
   std::random_device seed ;
   // generator
   std::mt19937 engine( seed( ) ) ;
   // number distribution
   std::uniform_int_distribution<int> choose( 0 , numbers.size( ) - 1 ) ;
   std::cout << "random element picked : " << numbers[ choose( engine ) ]
      << " !\n" ;
   return 0 ;
}
