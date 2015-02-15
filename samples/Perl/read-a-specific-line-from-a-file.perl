#!/usr/bin/perl -s
# invoke as <scriptname> -n=7 [input]
while (<>) { $. == $n and print, exit }
die "file too short\n";
