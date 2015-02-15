#!/usr/bin/perl -w
use strict ;
use GD ;

my $image = new GD::Image( 320 , 240 ) ;
my %colors = ( "white" => [ 255 , 255 , 255 ] , "red" => [255 , 0 , 0 ] ,
      "green" => [ 0 , 255 , 0 ] , "blue" => [ 0 , 0 , 255 ] ,
      "magenta" => [ 255 , 0 , 255 ] , "yellow" => [ 255 , 255 , 0 ] ,
      "cyan" => [ 0 , 255 , 255 ] , "black" => [ 0 , 0 , 0 ] ) ;
my @paintcolors ;
foreach my $color ( keys %colors ) {
   my $paintcolor = $image->colorAllocate( @{$colors{ $color }} ) ;
   push @paintcolors, $paintcolor ;
}
my $startx = 0 ;
my $starty = 0 ;
my $run = 0 ;
my $barheight = 240 / 4 ;
my $colorindex = 0 ;
while ( $run < 4 ) {
   my $barwidth =  $run + 1 ;
   while ( $startx + $barwidth < 320 ) {
      $image->filledRectangle( $startx , $starty , $startx + $barwidth ,
	    $starty + $barheight - 1 , $paintcolors[ $colorindex % 8 ] ) ;
      $startx += $barwidth ;
      $colorindex++ ;
   }
   $starty += $barheight ;
   $startx = 0 ;
   $colorindex = 0 ;
   $run++ ;
 }
open ( DISPLAY , ">" , "pinstripes.png" ) || die ;
binmode DISPLAY ;
print DISPLAY $image->png ;
close DISPLAY ;
