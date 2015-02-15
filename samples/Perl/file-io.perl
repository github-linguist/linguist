#!/usr/bin/perl

open my $fh_in, '<', 'input.txt' or die "could not open <input.txt> for reading: $!";
open my $fh_out, '>', 'output.txt' or die "could not open <output.txt> for writing: $!";
# '>' overwrites file, '>>' appends to file, just like in the shell

binmode $fh_out; # marks filehandle for binary content on systems where that matters

print $fh_out $_ while <$fh_in>;
# prints current line to file associated with $fh_out filehandle

# the same, less concise
#while (<$fh_in>) {
#  print $fh_out $_;
#};

close $fh_in;
close $fh_out;
