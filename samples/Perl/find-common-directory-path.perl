use List::Util qw(max reduce);
sub compath {
    my ($sep, @paths, %hash) = @_;
    # Tokenize and tally subpaths
    foreach (@paths) {
        my @tok = split $sep, substr($_,1);
        ++$hash{join $sep, @tok[0..$_]} for (0..$#tok); }
    # Return max length subpath or null
    my $max = max values %hash;
    return '' unless $max == @paths;
    my @res =  grep {$hash{$_} == $max} keys %hash;
    return $sep . reduce { length $a > length $b ? $a : $b } @res;
}

# Test and display
my @paths = qw(/home/user1/tmp/coverage/test
               /home/user1/tmp/covert/operator
               /home/user1/tmp/coven/members);
print compath('/', @paths), "\n";
