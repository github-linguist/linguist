use List::MoreUtils 'all';

sub pangram {all {$_[0] =~ /$_/i} 'a' .. 'z';}

print "Yes.\n" if pangram 'Cozy lummox gives smart squid who asks for job pen.';
