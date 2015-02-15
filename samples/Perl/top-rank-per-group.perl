sub zip {
    my @a = @{shift()};
    my @b = @{shift()};
    my @l;
    push @l, shift @a, shift @b while @a and @b;
    return @l;
}

sub uniq {
    my %h;
    grep {!$h{$_}++} @_;
}

my @data =
    map {{ zip [qw(name id salary dept)], [split ','] }}
    split "\n",
    <<'EOF';
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
EOF

@ARGV or die "Please provide a value for N.\n";
my $N = shift;

foreach my $d (sort uniq map {$_->{dept}} @data) {
    print "$d\n";
    my @es =
        sort {$b->{salary} <=> $a->{salary}}
        grep {$_->{dept} eq $d}
        @data;
    foreach (1 .. $N) {
        @es or last;
        my $e = shift @es;
        printf "%-15s | %-6s | %5d\n", @{$e}{qw(name id salary)};
    }
    print "\n";
}
