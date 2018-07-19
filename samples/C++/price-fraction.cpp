#include <iostream>
#include <cmath>

int main( ) {
   double froms[ ] = { 0.00 , 0.06 , 0.11 , 0.16 , 0.21 , 0.26 ,
       0.31 , 0.36 , 0.41 , 0.46 , 0.51 , 0.56 , 0.61 , 0.66 ,
       0.71 , 0.76 , 0.81 , 0.86 , 0.91 , 0.96 } ;
   double tos[ ] = { 0.06 , 0.11 , 0.16 , 0.21 , 0.26 , 0.31 ,
      0.36 , 0.41 , 0.46 , 0.51 , 0.56 , 0.61 , 0.66 , 0.71 ,
      0.76 , 0.81 , 0.86 , 0.91 , 0.96 , 1.01 } ;
   double replacements [] = { 0.10 , 0.18 , 0.26 , 0.32 , 0.38 ,
      0.44 , 0.50 , 0.54 , 0.58 , 0.62 , 0.66 , 0.70 , 0.74 ,
      0.78 , 0.82 , 0.86 , 0.90 , 0.94 , 0.98 , 1.00 } ;
   double number = 0.1 ;
   std::cout << "Enter a fractional number between 0 and 1 ( 0 to end )!\n" ;
   std::cin >> number ;
   while ( number != 0 ) {
      if ( number < 0 || number > 1 ) {
	 std::cerr << "Error! Only positive values between 0 and 1 are allowed!\n" ;
	 return 1 ;
      }
      int n = 0 ;
      while ( ! ( number >= froms[ n ] && number < tos[ n ] ) )
	 n++ ;
      std::cout << "-->" << replacements[ n ] << '\n' ;
      std::cout << "Enter a fractional number ( 0 to end )!\n" ;
      std::cin >> number ;
   }
   return 0 ;
}
