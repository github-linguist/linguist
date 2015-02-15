#!/usr/bin/perl

use strict; use warnings;
use Date::Calc qw(:all);

my @abbr = qw( Not Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );

my %c_hols = (
 	Easter=>     0,
 	Ascension=> 39,
 	Pentecost=> 49,
 	Trinity=>   56,
 	Corpus=>    60
);

sub easter {
	my $year=shift;
	
	my $ay=$year % 19;
	my $by=int($year / 100);
	my $cy=$year % 100;
	my $dy=int($by/4);
	my $ey=$by % 4;
	my $fy=int(($by+8)/25);
	my $gy=int(($by-$fy+1)/3);
	my $hy=($ay*19+$by-$dy-$gy+15) % 30;
	my $iy=int($cy/4);
	my $ky=$cy % 4;
	my $ly=(32+2*$ey+2*$iy-$hy-$ky) % 7;
	my $m_y=int(($ay+11*$hy+22*$ly)/451);
	
	my $month=int(($hy+$ly-7*$m_y+114)/31);
	my $day=(($hy+$ly-7*$m_y+114) % 31)+1;
	
	return ($month, $day, $year);
}

sub cholidays {
	my $year=shift;
	my ($emon, $eday)=easter($year);
	my @fields;
	printf("%4s: ", $year);
	
	foreach my $hol (sort { $c_hols{$a}<=>$c_hols{$b} } keys %c_hols) {
		my ($ye,$mo,$da)=Add_Delta_Days($year,$emon,$eday,$c_hols{$hol});
		my $month=$abbr[$mo];
		push @fields, sprintf("%s: %02s %s",$hol,$da,$month);
	}
	print join (", ",@fields);
	print "\n";
}


print 	"Christian holidays, related to Easter, for each centennial from ",
		"400 to 2100 CE:\n";
for (my $year=400; $year<=2100; $year+=100) {
	cholidays($year);
}

print	"Christian holidays, related to Easter, ",
		"for years from 2010 to 2020 CE:\n";


cholidays($_) for(2010..2020);
