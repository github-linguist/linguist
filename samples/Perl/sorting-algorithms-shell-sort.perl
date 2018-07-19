sub shell_sort {
    my (@a, $h, $i, $j, $k) = @_;
    for ($h = @a; $h = int $h / 2;) {
        for $i ($h .. $#a) {
            $k = $a[$i];
            for ($j = $i; $j >= $h && $k < $a[$j - $h]; $j -= $h) {
                $a[$j] = $a[$j - $h];
            }
            $a[$j] = $k;
        }
    }
    @a;
}

my @a = map int rand 100, 1 .. $ARGV[0] || 10;
say "@a";
@a = shell_sort @a;
say "@a";
