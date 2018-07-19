my @c = '##';
@c = (map($_ x 3, @c), map($_.(' ' x length).$_, @c), map($_ x 3, @c))
        for 1 .. 3;
print join("\n", @c), "\n";
