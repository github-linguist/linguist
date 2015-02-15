use strict;
use warnings;
use Tk;
use Math::Trig qw/:pi/;

my $root =  new MainWindow( -title => 'Pendulum Animation' );
my $canvas = $root->Canvas(-width => 320, -height => 200);
my $after_id;

for ($canvas) {
	$_->createLine(   0,  25, 320,  25, -tags => [qw/plate/], -width => 2, -fill => 'grey50' );
	$_->createOval( 155,  20, 165,  30, -tags => [qw/pivot outline/], -fill => 'grey50' );
	$_->createLine(   1,   1,    1,  1, -tags => [qw/rod width/], -width => 3, -fill => 'black' );
	$_->createOval(   1,   1,    2,  2, -tags => [qw/bob outline/], -fill => 'yellow' );
}

$canvas->raise('pivot');
$canvas->pack(-fill => 'both', -expand => 1);
my ($Theta, $dTheta, $length, $homeX, $homeY) =
	(45, 0, 150, 160, 25);

sub show_pendulum {
  my $angle = $Theta * pi() / 180;
  my $x = $homeX + $length * sin($angle);
  my $y = $homeY + $length * cos($angle);
  $canvas->coords('rod', $homeX, $homeY, $x, $y);
  $canvas->coords('bob', $x-15, $y-15, $x+15, $y+15);
}



sub recompute_angle {
  my $scaling = 3000.0 / ($length ** 2);
  # first estimate
  my $firstDDTheta = -sin($Theta * pi / 180) * $scaling;
  my $midDTheta = $dTheta + $firstDDTheta;
  my $midTheta = $Theta + ($dTheta + $midDTheta)/2;
  # second estimate
  my $midDDTheta = -sin($midTheta * pi/ 180) * $scaling;
  $midDTheta = $dTheta + ($firstDDTheta + $midDDTheta)/2;
  $midTheta = $Theta + ($dTheta + $midDTheta)/2;
  # again, first
  $midDDTheta = -sin($midTheta * pi/ 180) * $scaling;
  my $lastDTheta = $midDTheta + $midDDTheta;
  my $lastTheta = $midTheta + ($midDTheta + $lastDTheta)/2;
  # again, second
  my $lastDDTheta = -sin($lastTheta * pi/180) * $scaling;
  $lastDTheta = $midDTheta + ($midDDTheta + $lastDDTheta)/2;
  $lastTheta = $midTheta + ($midDTheta + $lastDTheta)/2;
  # Now put the values back in our globals
  $dTheta  = $lastDTheta;
  $Theta = $lastTheta;
}


sub animate {
  recompute_angle;
  show_pendulum;
  $after_id = $root->after(15 => sub {animate() });
}

show_pendulum;
$after_id = $root->after(500 => sub {animate});

$canvas->bind('<Destroy>' => sub {$after_id->cancel});
MainLoop;
