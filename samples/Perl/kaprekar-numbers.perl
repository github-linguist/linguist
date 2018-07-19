sub is_kaprekar {
        my $n = shift;
        return 1 if $n == 1;
        my $s = $n * $n;
        for (1 .. length($s)) {
                return 1 if substr($s, 0, $_) + (0 + substr($s, $_) || return) == $n;
        }
}

# one million is a small number, let's brute force it
is_kaprekar($_) and print "$_\n" for 1 .. 1_000_000;
