#!/usr/bin/perl -w
use strict ;
use DateTime ;
use feature qw( say ) ;

foreach my $month ( 1..12 ) {
   my $dt = DateTime->last_day_of_month( year => $ARGV[ 0 ] , month => $month ) ;
   while ( $dt->day_of_week != 5 ) {
      $dt->subtract( days => 1 ) ;
   }
   say $dt->ymd ;
}
