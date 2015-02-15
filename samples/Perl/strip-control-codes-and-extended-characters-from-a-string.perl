#!/usr/bin/perl -w
use strict ;

my @letters ;
my @nocontrols ;
my @noextended ;
for ( 1..40 ) {
   push @letters ,  int( rand( 256 ) ) ;
}
print "before sanitation : " ;
print join( '' , map { chr( $_ ) } @letters ) ;
print "\n" ;
@nocontrols = grep { $_ > 32 && $_ != 127 } @letters ;
print "Without controls: " ;
print join( '' , map { chr( $_ ) } @nocontrols ) ;
@noextended = grep { $_ < 127 } @nocontrols ;
print "\nWithout extended: " ;
print join( '' , map { chr( $_ ) } @noextended ) ;
print "\n" ;
