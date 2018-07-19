use strict;
my @c = (); # create an empty "array" collection

# fill it
push @c, 10, 11, 12;
push @c, 65;
# print it
print join(" ",@c) . "\n";

# create an empty hash
my %h = ();
# add some pair
$h{'one'} = 1;
$h{'two'} = 2;
# print it
foreach my $i ( keys %h ) {
    print $i . " -> " . $h{$i} . "\n";
}
