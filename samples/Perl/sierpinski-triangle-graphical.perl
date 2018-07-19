use List::Util qw'min max sum';

sub write_eps {
	my @x = @_[0, 2, 4];
	my @y = @_[1, 3, 5];
	my $sx = sum(@x) / 3;
	my $sy = sum(@y) / 3;
	@x = map { $_ - $sx } @x;
	@y = map { $_ - $sy } @y;

	print <<"HEAD";
%!PS-Adobe-3.0
%%BoundingBox: @{[min(@x) - 10]} @{[min(@y) - 10]} @{[max(@x) + 10]} @{[max(@y) + 10]}
/v1 { $x[0] $y[0] } def /v2 { $x[1] $y[1] } def /v3 { $x[2] $y[2] } def
/t { translate } def
/r { .5 .5 scale 2 copy t 2 index sierp pop neg exch neg exch t 2 2 scale } def

/sierp { dup 1 sub dup 0 ne
	{ v1 r v2 r v3 r }
	{ v1 moveto v2 lineto v3 lineto} ifelse
	pop
} def

9 sierp fill pop showpage
%%EOF
HEAD
}

write_eps 0, 0, 300, 215, -25, 200;
