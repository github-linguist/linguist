#include <QByteArray>
#include <iostream>

int main( ) {
   QByteArray text ( "http://foo bar/" ) ;
   QByteArray encoded( text.toPercentEncoding( ) ) ;
   std::cout << encoded.data( ) << '\n' ;
   return 0 ;
}
