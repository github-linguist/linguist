sub sierpinski {
    my ($n) = @_;
    my @down = '*';
    my $space = ' ';
    foreach (1..$n) {
        @down = (map("$space$_$space", @down), map("$_ $_", @down));
        $space = "$space$space";
    }
    return @down;
}

print "$_\n" foreach sierpinski 4;
