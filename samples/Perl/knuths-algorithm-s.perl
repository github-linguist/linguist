use strict;

sub s_of_n_creator {
    my $n = shift;
    my @sample;
    my $i = 0;
    sub {
        my $item = shift;
        $i++;
        if ($i <= $n) {
            # Keep first n items
            push @sample, $item;
        } elsif (rand() < $n / $i) {
            # Keep item
            @sample[rand $n] = $item;
        }
        @sample
    }
}

my @items = (0..9);
my @bin;

foreach my $trial (1 .. 100000) {
    my $s_of_n = s_of_n_creator(3);
    my @sample;
    foreach my $item (@items) {
        @sample = $s_of_n->($item);
    }
    foreach my $s (@sample) {
        $bin[$s]++;
    }
}
print "@bin\n";
