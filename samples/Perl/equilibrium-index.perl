sub eq_index {
    my ( $i, $sum, %sums ) = ( 0, 0 );

    for (@_) {
        push @{ $sums{ $sum * 2 + $_  } }, $i++;
        $sum += $_;
    }

    return join ' ', @{ $sums{$sum} || [] }, "\n";
}

print eq_index qw(  -7  1  5  2 -4  3  0 ); # 3 6
print eq_index qw(   2  4  6             ); # (no eq point)
print eq_index qw(   2  9  2             ); # 1
print eq_index qw(   1 -1  1 -1  1 -1  1 ); # 0 1 2 3 4 5 6
