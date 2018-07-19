#!/usr/bin/perl -w
use DateTime ;

my @happymonths ;
my @workhardyears ;
my @longmonths = ( 1 , 3 , 5 , 7 , 8 , 10 , 12 ) ;
my @years = 1900..2100 ;
foreach my $year ( @years ) {
   my $countmonths = 0 ;
   foreach my $month ( @longmonths ) {
      my $dt = DateTime->new( year => $year ,
	                      month => $month ,
			      day   => 1 ) ;
      if ( $dt->day_of_week == 5 ) {
	 $countmonths++ ;
	 my $yearfound = $dt->year ;
	 my $monthfound = $dt->month_name ;
	 push ( @happymonths , "$yearfound  $monthfound" ) ;
      }
   }
   if ( $countmonths == 0 ) {
      push ( @workhardyears, $year ) ;
   }
}
print "There are " . @happymonths . " months with 5 full weekends!\n" ;
print "The first 5 and the last 5 of them are:\n" ;
foreach my $i ( 0..4 ) {
   print "$happymonths[ $i ]\n" ;
}
foreach my $i ( -5..-1 ) {
   print "$happymonths[ $i ]\n" ;
}
print "No long weekends in the following " . @workhardyears . " years:\n" ;
map { print "$_\n" } @workhardyears ;
