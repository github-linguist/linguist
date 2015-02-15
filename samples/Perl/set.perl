use strict;

package Set; # likely will conflict with stuff on CPAN
use overload
	'""'	=> \&str,
	'bool'	=> \&count,
	'+='	=> \&add,
	'-='	=> \&del,
	'-'	=> \&diff,
	'=='	=> \&eq,
	'&'	=> \&intersection,
	'|'	=> \&union,
	'^'	=> \&xdiff;

sub str {
	my $set = shift;
	# This has drawbacks: stringification is used as set key
	# if the set is added to another set as an element, which
	# may cause inconsistencies if the element set is modified
	# later.  In general, a hash key loses its object identity
	# anyway, so it's not unique to us.
	"Set{ ".  join(", " => sort map("$_", values %$set)) . " }"
}

sub new {
	my $pkg = shift;
	my $h = bless {};
	$h->add($_) for @_;
	$h
}

sub add {
	my ($set, $elem) = @_;
	$set->{$elem} = $elem;
	$set
}

sub del {
	my ($set, $elem) = @_;
	delete $set->{$elem};
	$set
}

sub has { # set has element
	my ($set, $elem) = @_;
	exists $set->{$elem}
}

sub union {
	my ($this, $that) = @_;
	bless { %$this, %$that }
}

sub intersection {
	my ($this, $that) = @_;
	my $s = new Set;
	for (keys %$this) {
		$s->{$_} = $this->{$_} if exists $that->{$_}
	}
	$s
}

sub diff {
	my ($this, $that) = @_;
	my $s = Set->new;
	for (keys %$this) {
		$s += $this->{$_} unless exists $that->{$_}
	}
	$s
}

sub xdiff { # xor, symmetric diff
	my ($this, $that) = @_;
	my $s = new Set;
	bless { %{ ($this - $that) | ($that - $this) } }
}

sub count { scalar(keys %{+shift}) }

sub eq {
	my ($this, $that) = @_;
	!($this - $that) && !($that - $this);
}

sub contains { # this is a superset of that
	my ($this, $that) = @_;
	for (keys %$that) {
		return 0 unless $this->has($_)
	}
	return 1
}

package main;
my ($x, $y, $z, $w);

$x = Set->new(1, 2, 3);
$x += $_ for (5 .. 7);
$y = Set->new(1, 2, 4, $x); # not the brightest idea

print "set x is: $x\nset y is: $y\n";
for (1 .. 4, $x) {
	print "$_ is", $y->has($_) ? "" : " not", " in y\n";
}

print "union: ", $x | $y, "\n";
print "intersect: ", $x & $y, "\n";
print "z = x - y = ", $z = $x - $y, "\n";
print "y is ", $x->contains($y) ? "" : "not ", "a subset of x\n";
print "z is ", $x->contains($z) ? "" : "not ", "a subset of x\n";
print "z = (x | y) - (x & y) = ", $z = ($x | $y) - ($x & $y), "\n";
print "w = x ^ y = ", $w = ($x ^ $y), "\n";
print "w is ", ($w == $z) ? "" : "not ", "equal to z\n";
print "w is ", ($w == $x) ? "" : "not ", "equal to x\n";
