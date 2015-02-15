#!/usr/bin/perl
use strict ;
use warnings ;

sub middlethree {
   my $number = shift ;
   my $testnumber = abs $number ;
   my $error = "Middle 3 digits can't be shown" ;
   my $numberlength = length $testnumber ;
   if ( $numberlength < 3 ) {
      print "$error : $number too short!\n" ;
      return ;
   }
   if ( $numberlength % 2 == 0 ) {
      print "$error : even number of digits in $number!\n" ;
      return ;
   }
   my $middle = int ( $numberlength  / 2 ) ;
   print "Middle 3 digits of $number : " . substr( $testnumber , $middle - 1 , 3 ) . " !\n" ;
   return ;
}

my @numbers = ( 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345 ,
      1, 2, -1, -10, 2002, -2002, 0 ) ;
map { middlethree( $_ ) } @numbers ;
