#! /usr/bin/perl
use strict;

my %pairs = ( "hello" => 13,
	      "world" => 31,
	      "!" => 71 );

# iterate over pairs

# Be careful when using each(), however, because it uses a global iterator
# associated with the hash. If you call keys() or values() on the hash in the
# middle of the loop, the each() iterator will be reset to the beginning. If
# you call each() on the hash somewhere in the middle of the loop, it will
# skip over elements for the "outer" each(). Only use each() if you are sure
# that the code inside the loop will not call keys(), values(), or each().
while ( my ($k, $v) = each %pairs) {
    print "(k,v) = ($k, $v)\n";
}

# iterate over keys
foreach my $key ( keys %pairs ) {
    print "key = $key, value = $pairs{$key}\n";
}
# or (see note about each() above)
while ( my $key = each %pairs) {
    print "key = $key, value = $pairs{$key}\n";
}

# iterate over values
foreach my $val ( values %pairs ) {
    print "value = $val\n";
}
