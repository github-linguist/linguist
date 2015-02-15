# create array
my @a = (1, 2, 3, 4, 5);

# create callback function
sub mycallback {
  return 2 * shift;
}

# use array indexing
for (my $i = 0; $i < scalar @a; $i++) {
  print "mycallback($a[$i]) = ", mycallback($a[$i]), "\n";
}

# using foreach
foreach my $x (@a) {
  print "mycallback($x) = ", mycallback($x), "\n";
}

# using map (useful for transforming an array)
my @b = map mycallback($_), @a;                # @b is now (2, 4, 6, 8, 10)

# and the same using an anonymous function
my @c = map { $_ * 2 } @a;                     # @c is now (2, 4, 6, 8, 10)

# use a callback stored in a variable
my $func = \&mycallback;
my @d = map $func->($_), @a;                  # @d is now (2, 4, 6, 8, 10)

# filter an array
my @e = grep { $_ % 2 == 0 } @a;               # @e is now (2, 4)
