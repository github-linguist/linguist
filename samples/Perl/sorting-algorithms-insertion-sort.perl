sub insertion_sort {
    my (@list) = @_;
    foreach my $i (1 .. $#list) {
        my $j = $i;
        my $k = $list[$i];
        while ( $j > 0 && $k < $list[$j - 1]) {
            $list[$j] = $list[$j - 1];
            $j--;
        }
        $list[$j] = $k;
    }
    return @list;
}

my @a = insertion_sort(4, 65, 2, -31, 0, 99, 83, 782, 1);
print "@a\n";
