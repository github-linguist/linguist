#!perl
use 5.010;
use strict;
use warnings qw(FATAL all);

my $balanced = qr{([^<>,]++|<(?-1),(?-1),(?-1),(?-1)>)};
my ($a, $b, $c, $d, $x, $y, $z) = map +qr((?<$_>$balanced)),
	'a'..'d', 'x'..'z';
my $col = qr{(?<col>[RB])};

sub balance {
	local $_ = shift;
	if( /^<B,<R,<R,$a,$x,$b>,$y,$c>,$z,$d>\z/ or
		/^<B,<R,$a,$x,<R,$b,$y,$c>>,$z,$d>\z/ or
		/^<B,$a,$x,<R,<R,$b,$y,$c>,$z,$d>>\z/ or
		/^<B,$a,$x,<R,$b,$y,<R,$c,$z,$d>>>\z/ )
	{
		my ($aa, $bb, $cc, $dd) = @+{'a'..'d'};
		my ($xx, $yy, $zz) = @+{'x'..'z'};
		"<R,<B,$aa,$xx,$bb>,$yy,<B,$cc,$zz,$dd>>";
	} else {
		$_;
	}
}

sub ins {
	my ($xx, $tree) = @_;
	if($tree =~ m{^<$col,$a,$y,$b>\z} ) {
		my ($color, $aa, $bb, $yy) = @+{qw(col a b y)};
		if( $xx < $yy ) {
			return balance "<$color,".ins($xx,$aa).",$yy,$bb>";
		} elsif( $xx > $yy ) {
			return balance "<$color,$aa,$yy,".ins($xx,$bb).">";
		} else {
			return $tree;
		}
	} elsif( $tree !~ /,/) {
		return "<R,_,$xx,_>";
	} else {
		print "Unexpected failure!\n";
		print "Tree parts are: \n";
		print $_, "\n" for $tree =~ /$balanced/g;
		exit;
	}
}

sub insert {
	my $tree = ins(@_);
	$tree =~ m{^<$col,$a,$y,$b>\z} or die;
	"<B,$+{a},$+{y},$+{b}>";
}

MAIN: {
	my @a = 1..10;
	for my $aa ( 1 .. $#a ) {
		my $bb = int rand( 1 + $aa );
		@a[$aa, $bb] = @a[$bb, $aa];
	}
	my $t = "!";
	for( @a ) {
		$t = insert( $_, $t );
		print "Tree: $t.\n";
	}
}
print "Done\n";
