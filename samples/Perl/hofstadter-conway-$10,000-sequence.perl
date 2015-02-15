#!/usr/bin/perl
use warnings ;
use strict ;

my $limit = 2 ** 20 ;
my @numbers = ( 0 , 1 , 1 ) ;
my $mallows ;
my $max_i ;
foreach my $i ( 3..$limit ) {
   push ( @numbers , $numbers[ $numbers[ $i - 1 ]] + $numbers[ $i - $numbers[ $i - 1 ] ] ) ;
}
for ( my $rangelimit = 1 ; $rangelimit < 20 ; $rangelimit++ ) {
   my $max = 0 ;
   for ( my $i = 2 ** $rangelimit ; $i < ( 2 ** ( $rangelimit + 1 ) ) ; $i++ ) {
      my $rat = $numbers[ $i ] / $i ;
      $mallows = $i if $rat >= 0.55 ;
      if ( $rat > $max ) {
	 $max = $rat ;
	 $max_i = $i ;
      }
   }
   my $upperlimit = $rangelimit + 1 ;
   print "Between 2 ^ $rangelimit and 2 ^ $upperlimit the maximum value is $max at $max_i !\n" ;
}
print "The prize would have been won at $mallows !\n"
