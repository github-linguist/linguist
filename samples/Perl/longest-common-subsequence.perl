use Algorithm::Diff qw/ LCS /;

my @a = split //, 'thisisatest';
my @b = split //, 'testing123testing';

print LCS( \@a, \@b );
