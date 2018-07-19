sub merge_sort {
    my @x = @_;
    return @x if @x < 2;
    my $m = int @x / 2;
    my @a = merge_sort(@x[0 .. $m - 1]);
    my @b = merge_sort(@x[$m .. $#x]);
    for (@x) {
        $_ = !@a            ? shift @b
           : !@b            ? shift @a
           : $a[0] <= $b[0] ? shift @a
           :                  shift @b;
    }
    @x;
}

my @a = (4, 65, 2, -31, 0, 99, 83, 782, 1);
@a = merge_sort @a;
print "@a\n";
