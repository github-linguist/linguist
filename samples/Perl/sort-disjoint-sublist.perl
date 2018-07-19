#!/usr/bin/perl -w
use strict ;

# this function sorts the array in place
sub disjointSort {
   my ( $values , @indices ) = @_ ;

   @{$values}[ sort @indices ] = sort @{$values}[ @indices ] ;
}

my @values =  ( 7 , 6 , 5 , 4 , 3 , 2 , 1 , 0 ) ;
my @indices = ( 6 , 1 , 7 ) ;
disjointSort( \@values , @indices ) ;
print "[@values]\n" ;
