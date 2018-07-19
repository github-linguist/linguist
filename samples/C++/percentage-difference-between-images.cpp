#include <QImage>
#include <cstdlib>
#include <QColor>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   if ( argc != 3 ) {
      std::cout << "Call this with imagecompare <file of image 1>"
	 << " <file of image 2>\n" ;
      return 1 ;
   }
   QImage firstImage ( argv[ 1 ] ) ;
   QImage secondImage ( argv[ 2 ] ) ;
   double totaldiff = 0.0 ; //holds the number of different pixels
   int h = firstImage.height( ) ;
   int w = firstImage.width( ) ;
   int hsecond = secondImage.height( ) ;
   int wsecond = secondImage.width( ) ;
   if ( w != wsecond || h != hsecond ) {
      std::cerr << "Error, pictures must have identical dimensions!\n" ;
      return 2 ;
   }
   for ( int y = 0 ; y < h ; y++ ) {
      uint *firstLine = ( uint* )firstImage.scanLine( y ) ;
      uint *secondLine = ( uint* )secondImage.scanLine( y ) ;
      for ( int x = 0 ; x < w ; x++ ) {
	 uint pixelFirst = firstLine[ x ] ;
	 int rFirst = qRed( pixelFirst ) ;
	 int gFirst = qGreen( pixelFirst ) ;
	 int bFirst = qBlue( pixelFirst ) ;
	 uint pixelSecond = secondLine[ x ] ;
	 int rSecond = qRed( pixelSecond ) ;
	 int gSecond = qGreen( pixelSecond ) ;
	 int bSecond = qBlue( pixelSecond ) ;
	 totaldiff += std::abs( rFirst - rSecond ) / 255.0 ;
	 totaldiff += std::abs( gFirst - gSecond ) / 255.0 ;
	 totaldiff += std::abs( bFirst -bSecond ) / 255.0 ;
      }
   }
   std::cout << "The difference of the two pictures is " <<
      (totaldiff * 100)  / (w * h * 3)  << " % !\n" ;
   return 0 ;
}
