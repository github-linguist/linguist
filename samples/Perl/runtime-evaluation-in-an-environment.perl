sub eval_with_x
   {my $code = shift;
    my $x = shift;
    my $first = eval $code;
    $x = shift;
    return eval($code) - $first;}

print eval_with_x('3 * $x', 5, 10), "\n"; # Prints "15".
