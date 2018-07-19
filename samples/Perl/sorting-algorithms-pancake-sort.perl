sub pancake {
        my @x = @_;
        for my $idx (0 .. $#x - 1) {
                my $min = $idx;
                $x[$min] > $x[$_] and $min = $_           for $idx + 1 .. $#x;

                next if $x[$min] == $x[$idx];

                @x[$min .. $#x] = reverse @x[$min .. $#x] if $x[$min] != $x[-1];
                @x[$idx .. $#x] = reverse @x[$idx .. $#x];
        }
        @x;
}

my @a = map (int rand(100), 1 .. 10);
print "Before @a\n";
@a = pancake(@a);
print "After  @a\n";
