sub flatten {
    map { ref eq 'ARRAY' ? flatten(@$_) : $_ } @_
}

my @lst = ([1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []);
print flatten(@lst), "\n";
