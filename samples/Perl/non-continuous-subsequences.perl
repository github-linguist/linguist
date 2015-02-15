my ($max, @current);
sub non_continuous {
        my ($idx, $has_gap, $found) = @_;

        for ($idx .. $max) {
                push @current, $_;
                # print "@current\n" if $has_gap; # uncomment for huge output
                $found ++ if $has_gap;
                $found += non_continuous($_ + 1, $has_gap)   if $_ < $max;
                pop @current;
                $has_gap = @current;   # don't set gap flag if it's empty still
        }
        $found;
}

$max = 20;      # 1048365 sequences, 10 seconds-ish
print "found ", non_continuous(1), " sequences\n";
