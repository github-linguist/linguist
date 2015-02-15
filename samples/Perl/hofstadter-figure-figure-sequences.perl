#!perl
use strict;
use warnings;

my @r = ( undef, 1 );
my @s = ( undef, 2 );

sub ffsr {
  my $n = shift;
  while( $#r < $n ) {
    push @r, $s[$#r]+$r[-1];
    push @s, grep { $s[-1]<$_ } $s[-1]+1..$r[-1]-1, $r[-1]+1;
  }
  return $n;
}

sub ffr { $r[ffsr shift] }
sub ffs { $s[ffsr shift] }

printf "  i: R(i) S(i)\n";
printf "==============\n";
printf "%3d:  %3d  %3d\n", $_, ffr($_), ffs($_) for 1..10;
printf "\nR(40)=%3d S(960)=%3d R(41)=%3d\n", ffr(40), ffs(960), ffr(41);

my %seen;
$seen{ffr($_)}++ for 1 .. 40;
$seen{ffs($_)}++ for 1 .. 960;
if( 1000 == keys %seen and grep $seen{$_}, 1 .. 1000 ) {
	print "All occured exactly once.\n";
} else {
	my @missed = grep !$seen{$_}, 1 .. 1000;
	my @dupped = sort { $a <=> $b} grep $seen{$_}>1, keys %seen;
	print "These were missed: @missed\n";
	print "These were duplicated: @dupped\n";
}
