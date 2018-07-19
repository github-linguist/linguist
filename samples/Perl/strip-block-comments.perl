#!/usr/bin/perl -w
use strict ;
use warnings ;

open( FH , "<" , "samplecode.txt" ) or die "Can't open file!$!\n" ;
my $code = "" ;
{
   local $/ ;
   $code = <FH> ; #slurp mode
}
close FH ;
$code =~ s,/\*.*?\*/,,sg ;
print $code . "\n" ;
