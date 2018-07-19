#!/usr/bin/perl
use strict ;
use warnings ;
use Digest::MD4  qw( md4_hex ) ;

print "Rosetta Code => " , md4_hex( "Rosetta Code" ) , "\n" ;
