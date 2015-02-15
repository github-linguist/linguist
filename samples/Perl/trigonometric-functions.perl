use Math::Trig;

my $angle_degrees = 45;
my $angle_radians = pi / 4;

print sin($angle_radians), ' ', sin(deg2rad($angle_degrees)), "\n";
print cos($angle_radians), ' ', cos(deg2rad($angle_degrees)), "\n";
print tan($angle_radians), ' ', tan(deg2rad($angle_degrees)), "\n";
print cot($angle_radians), ' ', cot(deg2rad($angle_degrees)), "\n";
my $asin = asin(sin($angle_radians));
print $asin, ' ', rad2deg($asin), "\n";
my $acos = acos(cos($angle_radians));
print $acos, ' ', rad2deg($acos), "\n";
my $atan = atan(tan($angle_radians));
print $atan, ' ', rad2deg($atan), "\n";
my $acot = acot(cot($angle_radians));
print $acot, ' ', rad2deg($acot), "\n";
