my (@names, @val, @weight, @vol, $max_vol, $max_weight, $vsc, $wsc);

if (1) { # change 1 to 0 for different data set
        @names  = qw(panacea    icor    gold);
        @val    = qw(3000       1800    2500);
        @weight = qw(3          2       20  );
        @vol    = qw(25         15      2   );
        $max_weight = 250;
        $max_vol = 250;
        $vsc = 1000;
        $wsc = 10;
} else { # with these numbers cache would have been useful
        @names  = qw(panacea    icor    gold    banana  monkey  );
        @val    = qw(17         11      5       3       34      );
        @weight = qw(14         3       2       2       10      );
        @vol    = qw(3          4       2       1       12      );
        $max_weight = 150;
        $max_vol = 100;
        $vsc = $wsc = 1;
}

my @cache;
my ($hits, $misses) = (0, 0);
sub solu {
        my ($i, $w, $v) = @_;
        return [0, []] if $i < 0;

        if ($cache[$i][$w][$v]) {
                $hits ++;
                return $cache[$i][$w][$v]
        }
        $misses ++;

        my $x = solu($i - 1, $w, $v);

        my ($w1, $v1);
        for (my $t = 1; ; $t++) {
                last if ($w1 = $w - $t * $weight[$i]) < 0;
                last if ($v1 = $v - $t * $vol[$i]) < 0;

                my $y = solu($i - 1, $w1, $v1);

                if ( (my $tmp = $y->[0] + $val[$i] * $t) > $x->[0] ) {
                        $x = [ $tmp, [ @{$y->[1]}, [$i, $t] ] ];
                }
        }

        $cache[$i][$w][$v] = $x
}

my $x = solu($#names, $max_weight, $max_vol);
print   "Max value $x->[0], with:\n",
        "    Item\tQty\tWeight   Vol    Value\n", '-'x 50, "\n";

my ($wtot, $vtot) = (0, 0);
for (@{$x->[1]}) {
        my $i = $_->[0];
        printf  "    $names[$i]:\t% 3d  % 8d% 8g% 8d\n",
                $_->[1],
                $weight[$i] * $_->[1] / $wsc,
                $vol[$i] * $_->[1] / $vsc,
                $val[$i] * $_->[1];

        $wtot += $weight[$i] * $_->[1];
        $vtot += $vol[$i] * $_->[1];
}
print   "-" x 50, "\n";
printf  "    Total:\t     % 8d% 8g% 8d\n",
        $wtot/$wsc, $vtot/$vsc, $x->[0];

print "\nCache hit: $hits\tmiss: $misses\n";
