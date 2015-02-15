#!/usr/bin/env perl

use strict;
use warnings;

sub main {
	my $program = $0;
	print "Program: $program\n";
}

unless(caller) { main; }
