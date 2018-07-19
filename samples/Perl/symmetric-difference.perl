sub symm_diff {
        # two lists passed in as references
        my %in_a = map(($_=>1), @{+shift});
        my %in_b = map(($_=>1), @{+shift});

        my @a = grep { !$in_b{$_} } keys %in_a;
        my @b = grep { !$in_a{$_} } keys %in_b;

        # return A-B, B-A, A xor B as ref to lists
        return \@a, \@b, [ @a, @b ]
}

my @a = qw(John Serena Bob  Mary Serena);
my @b = qw(Jim  Mary   John Jim  Bob   );

my ($a, $b, $s) = symm_diff(\@a, \@b);
print "A\\B: @$a\nB\\A: @$b\nSymm: @$s\n";
