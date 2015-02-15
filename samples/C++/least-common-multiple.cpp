#include <boost/math/common_factor.hpp>
#include <iostream>

int main( ) {
   std::cout << "The least common multiple of 12 and 18 is " <<
      boost::math::lcm( 12 , 18 ) << " ,\n"
      << "and the greatest common divisor " << boost::math::gcd( 12 , 18 ) << " !" << std::endl ;
   return 0 ;
}
