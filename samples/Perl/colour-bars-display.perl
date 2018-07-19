#!/usr/bin/perl -w
use strict ;
use GD ;

my %colors = ( white => [ 255 , 255 , 255 ] , red => [255 , 0 , 0 ] ,
      green => [ 0 , 255 , 0 ] , blue => [ 0 , 0 , 255 ] ,
      magenta => [ 255 , 0 , 255 ] , yellow => [ 255 , 255 , 0 ] ,
      cyan => [ 0 , 255 , 255 ] , black => [ 0 , 0 , 0 ] ) ;
my $barwidth = 160 / 8 ;
my $image = new GD::Image( 160 , 100 ) ;
my $start = 0 ;
foreach my $rgb ( values %colors ) {
   my $paintcolor = $image->colorAllocate( @$rgb ) ;
   $image->filledRectangle( $start * $barwidth , 0 , $start * $barwidth +
	 $barwidth - 1 , 99 , $paintcolor ) ;
   $start++ ;
}
open ( DISPLAY , ">" , "testprogram.png" ) || die ;
binmode DISPLAY ;
print DISPLAY $image->png ;
close DISPLAY ;#to be watched with <image viewer> testprogram.png
