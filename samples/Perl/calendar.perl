#!/usr/bin/perl
use strict;
use warnings;
use Time::Local;

my @names = qw/ JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC/;

my $year = shift ||'2007';

for my $month (0..11) {
    print " $names[$month] $year\n";
    print calendar($year, $month), "\n\n";
}

sub calendar {
    my ($year, $month) = @_;
    my @mon_days = qw/31 28 31 30 31 30 31 31 30 31 30 31/;
    ++$mon_days[1] if $year % 4 == 0 && ($year % 400 == 0 || $year % 1
+00 != 0);

    my $cal = " Sun Mon Tue Wed Thu Fri Sat\n";

    # Months are indexed beginning at 0
    my $time = timegm(0,0,0,1,$month,$year);
    my $wday = (gmtime $time)[6];

    $cal .= "    " x $wday;

    my $mday = 1;

    while ($mday <= $mon_days[$month]) {
        $cal .= sprintf "%4s", $mday++;
        $cal .= "\n" if ($wday + $mday -1) % 7 == 0;
    }
    return $cal;
}
# Let's use this as a placeholder until a better solution arrives, OK?
