package FPS;
use strict;
use warnings;
use Math::BigRat;

sub new {
   my $class = shift;
   return bless {@_}, $class unless @_ == 1;
   my $arg = shift;
   return bless { more => $arg }, $class if 'CODE' eq ref $arg;
   return bless { coeff => $arg }, $class if 'ARRAY' eq ref $arg;
   bless { coeff => [$arg] }, $class;
}

sub coeff {
   my ($self, $i) = @_;
   my $cache = ($self->{coeff} ||= []);
   my $more = $self->{more};
   for my $j ( @$cache .. $i ) {
      last unless $more;
      $cache->[$j] = $more->($j, $self);
   }
   $cache->[$i] or 0;
}

sub invert {
   my $orig = shift;
   ref($orig)->new( sub {
      my ($i, $self) = @_;
      unless( $i ) {
         my $a0 = $orig->coeff(0);
         die "Cannot invert power series with zero constant term."
            unless $a0;
         (Math::BigRat->new(1) / $a0);
      } else {
         my $sum = 0;
         my $terms = $self->{coeff};
         for my $j (1 .. $i) {
            $sum += $orig->coeff($j) * $terms->[$i - $j];
         }
         -$terms->[0] * $sum;
      }
   } );
}

sub fixargs {
   my ($x, $y, $swap) = @_;
   my $class = ref $x;
   $y = $class->new($y) unless UNIVERSAL::isa($y, $class);
   ($x, $y) = ($y, $x) if $swap;
   ($class, $x, $y);
}

use overload '+' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub { $x->coeff($_[0]) + $y->coeff($_[0]) } );
}, '-' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub { $x->coeff($_[0]) - $y->coeff($_[0]) } );
}, '*' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub {
      my $i = shift;
      my $sum = 0;
      $sum += $x->coeff($_) * $y->coeff($i-$_) for 0..$i;
      $sum;
   } );
}, '/' => sub {
   my ($class, $x, $y) = &fixargs;
   $x * $y->invert;
}, '""' => sub {
   my $self = shift;
   my $str = $self->coeff(0);
   for my $i (1..10) {
      my $c = $self->coeff($i);
      next unless $c;
      $str .= ($c < 0) ? (" - " . (-$c)) : (" + ".$c);
      $str .= "x^$i";
   }
   $str;
};

sub differentiate {
   my $orig = shift;
   ref($orig)->new( sub {
      my $i = shift;
      ($i+1) * $orig->coeff($i);
   } );
}

sub integrate {
   my $orig = shift;
   ref($orig)->new( coeff => [0], more => sub {
      my $i = shift;
      $orig->coeff($i-1) / Math::BigRat->new($i);
   } );
}

my $sin = __PACKAGE__->new;
my $cos = 1 - $sin->integrate;
%$sin = %{$cos->integrate};
my $tan = $sin / $cos;
my $exp = __PACKAGE__->new();
%$exp = (%{$exp->integrate}, coeff => [1]);

print "sin(x) ~= $sin\n";
print "cos(x) ~= $cos\n";
print "tan(x) ~= $tan\n";
print "exp(x) ~= $exp\n";

print "sin^2 + cos^s = ", $sin*$sin + $cos*$cos, "\n";

1;
__END__
