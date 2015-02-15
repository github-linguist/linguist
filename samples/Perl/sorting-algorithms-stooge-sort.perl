sub stooge {
        use integer;
        my ($x, $i, $j) = @_;

        $i //= 0;
        $j //= $#$x;

        if ( $x->[$j] < $x->[$i] ) {
                @$x[$i, $j] = @$x[$j, $i];
        }
        if ( $j - $i > 1 ) {
                my $t = ($j - $i + 1) / 3;
                stooge( $x, $i,      $j - $t );
                stooge( $x, $i + $t, $j      );
                stooge( $x, $i,      $j - $t );
        }
}


my @a = map (int rand(100), 1 .. 10);
print "Before @a\n";
stooge(\@a);
print "After  @a\n";
