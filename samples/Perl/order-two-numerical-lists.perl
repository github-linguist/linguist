#!/usr/bin/perl -w
use strict ;

sub orderlists {
   my $firstlist = shift ;
   my $secondlist = shift ;
   my $first = shift @{$firstlist } if @{$firstlist} ;
   my $second ;
#keep stripping elements from the first list as long as there are any
#or until the second list is used up!
   while ( @{$firstlist} ) {
      if ( @{$secondlist} ) { #second list is not used up yet!
	 $second = shift @{$secondlist} ;
	 if ( $first < $second ) {
	    return 1 ;
	 }
	 if ( $first > $second ) {
	    return 0 ;
	 }
      }
      else {                  #second list used up, defined to return false
	 return 0 ;
      }
      $first = shift @{$firstlist} ;
   }
   return 0 ;                 #in all remaining cases return false
}

my @firstnumbers = ( 43 , 33 , 2  ) ;
my @secondnumbers = ( 45 ) ;
if ( orderlists( \@firstnumbers , \@secondnumbers ) ) {
   print "The first list comes before the second list!\n" ;
}
else {
   print "The first list does not come before the second list!\n" ;
}
