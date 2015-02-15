sub beadsort {
    my @data = @_;

    my @columns;
    my @rows;

    for my $datum (@data) {
        for my $column ( 0 .. $datum-1 ) {
            ++ $rows[ $columns[$column]++ ];
        }
    }

    return reverse @rows;
}

beadsort 5, 7, 1, 3, 1, 1, 20;
