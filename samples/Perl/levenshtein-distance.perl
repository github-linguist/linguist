use List::Util 'min';

my %cache;

sub leven {
        my ($s, $t) = @_;
        return length($t) if !$s;
        return length($s) if !$t;

        $cache{$s}{$t} //=          # try commenting out this line
        do {
                my ($s1, $t1) = (substr($s, 1), substr($t, 1));

                (substr($s, 0, 1) eq substr($t, 0, 1))
                        ? leven($s1, $t1)
                        : 1 + min(leven($s1, $t1),
                                  leven($s,  $t1),
                                  leven($s1, $t ));
        };
}

print leven('rosettacode', 'raisethysword'), "\n";
