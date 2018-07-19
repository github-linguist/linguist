sub zip (&@)
{
        my $code = shift;
        my $min;
        $min = $min && $#$_ > $min ? $min : $#$_ for @_;

        for my $i(0..$min){ $code->(map $_->[$i] ,@_) }
}
my @a1 = qw( a b c );
my @a2 = qw( A B C );
my @a3 = qw( 1 2 3 );

zip { print @_,"\n" }\(@a1, @a2, @a3);
