use List::Util qw(shuffle);

my $turn = 0;
my @jumble = shuffle 1..9;

while ( join('', @jumble) eq '123456789' ) {
    @jumble = shuffle 1..9;
}

until ( join('', @jumble) eq '123456789' ) {
    $turn++;
    printf "%2d: @jumble - Flip how many digits ? ", $turn;

    my $d = <>;

    @jumble[0..$d-1] = reverse @jumble[0..$d-1];
}

print "    @jumble\n";
print "You won in $turn turns.\n";
