use strict;
use warnings;

my @d = qw( 0 + - );
my @v = qw( 0 1 -1 );

sub to_bt {
  my $n = shift;
  my $b = '';
  while( $n ) {
    my $r = $n%3;
    $b .= $d[$r];
    $n -= $v[$r];
    $n /= 3;
  }
  return scalar reverse $b;
}

sub from_bt {
  my $n = 0;
  for( split //, shift ) { # Horner
    $n *= 3;
    $n += "${_}1" if $_;
  }
  return $n;
}

my %addtable = (
                '-0' => [ '-', '' ],
                '+0' => [ '+', '' ],
                '+-' => [ '0', '' ],
                '00' => [ '0', '' ],
                '--' => [ '+', '-' ],
                '++' => [ '-', '+' ],
               );

sub add {
  my ($b1, $b2) = @_;
  return ($b1 or $b2 ) unless ($b1 and $b2);
  my $d = $addtable{ join '', sort substr( $b1, -1, 1, '' ), substr( $b2, -1, 1, '' ) };
  return add( add($b1, $d->[1]), $b2 ).$d->[0];
}

sub unary_minus {
  my $b = shift;
  $b =~ tr/-+/+-/;
  return $b;
}

sub subtract {
  my ($b1, $b2) = @_;
  return add( $b1, unary_minus $b2 );
}

sub mult {
  my ($b1, $b2) = @_;
  my $r = '0';
  for( reverse split //, $b2 ){
    $r = add $r, $b1      if $_ eq '+';
    $r = subtract $r, $b1 if $_ eq '-';
    $b1 .= '0';
  }
  $r =~ s/^0+//;
  return $r;
}

my $a = "+-0++0+";
my $b = to_bt( -436 );
my $c = "+-++-";
my $d = mult( $a, subtract( $b, $c ) );
printf "      a: %14s %10d\n", $a, from_bt( $a );
printf "      b: %14s %10d\n", $b, from_bt( $b );
printf "      c: %14s %10d\n", $c, from_bt( $c );
printf "a*(b-c): %14s %10d\n", $d, from_bt( $d );
