#include <iostream>

template< class T >
class D3Vector {

template< class U >
friend std::ostream & operator<<( std::ostream & , const D3Vector<U> & ) ;

public :
   D3Vector( T a , T b , T c ) {
      x = a ;
      y = b ;
      z = c ;
   }

   T dotproduct ( const D3Vector & rhs ) {
      T scalar = x * rhs.x + y * rhs.y + z * rhs.z ;
      return scalar ;
   }

   D3Vector crossproduct ( const D3Vector & rhs ) {
      T a = y * rhs.z - z * rhs.y ;
      T b = z * rhs.x - x * rhs.z ;
      T c = x * rhs.y - y * rhs.x ;
      D3Vector product( a , b , c ) ;
      return product ;
   }

   D3Vector triplevec( D3Vector & a , D3Vector & b ) {
      return crossproduct ( a.crossproduct( b ) ) ;
   }

   T triplescal( D3Vector & a, D3Vector & b ) {
      return dotproduct( a.crossproduct( b ) ) ;
   }

private :
   T x , y , z ;
} ;

template< class T >
std::ostream & operator<< ( std::ostream & os , const D3Vector<T> & vec ) {
   os << "( "  << vec.x << " ,  " << vec.y << " ,  " << vec.z << " )" ;
   return os ;
}

int main( ) {
   D3Vector<int> a( 3 , 4 , 5 ) , b ( 4 , 3 , 5 ) , c( -5 , -12 , -13 ) ;
   std::cout << "a . b : " << a.dotproduct( b ) << "\n" ;
   std::cout << "a x b : " << a.crossproduct( b ) << "\n" ;
   std::cout << "a . b x c : " << a.triplescal( b , c ) << "\n" ;
   std::cout << "a x b x c : " << a.triplevec( b , c ) << "\n" ;
   return 0 ;
}
