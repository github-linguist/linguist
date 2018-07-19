#!/usr/bin/perl -w
use strict ;

sub expon {
   my ( $base , $expo ) = @_ ;
   if ( $expo == 0 ) {
      return 1 ;
   }
   elsif ( $expo == 1 ) {
      return $base ;
   }
   elsif ( $expo > 1 ) {
      my $prod = 1 ;
      foreach my $n ( 0..($expo - 1) ) {
	 $prod *= $base ;
      }
      return $prod ;
   }
   elsif ( $expo < 0 ) {
      return 1 / ( expon ( $base , -$expo ) ) ;
   }
}
print "3 to the power of 10 as a function is " . expon( 3 , 10 ) . " !\n" ;
print "3 to the power of 10 as a builtin is " . 3**10 . " !\n" ;
print "5.5 to the power of -3 as a function is " . expon( 5.5 , -3 ) . " !\n" ;
print "5.5 to the power of -3 as a builtin is " . 5.5**-3 . " !\n" ;
