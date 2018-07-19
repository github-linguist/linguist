#!/usr/bin/perl
use strict ;
use warnings ;

my $number ;
my @sequence ;
print "Please enter 11 numbers!\n" ;
for my $i ( 0..10 ) {
   $number = <STDIN> ;
   chomp $number ;
   push @sequence , $number ;
}
map { my $result = sqrt( abs ( $_ ) ) + 5 * $_** 3 ; print "f( $_ ) " ; $result > 400 ? print "too large!\n" : print ": $result\n" ; }
   reverse @sequence ;
