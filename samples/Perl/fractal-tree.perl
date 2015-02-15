use GD::Simple;

my ($width, $height) = (1000,1000); # image dimension
my $scale = 6/10; # branch scale relative to trunk
my $length = 400; # trunk size

my $img = GD::Simple->new($width,$height);
$img->fgcolor('black');
$img->penSize(1,1);

tree($width/2, $height, $length, 270);

print $img->png;


sub tree
{
        my ($x, $y, $len, $angle) = @_;

        return if $len < 1;

        $img->moveTo($x,$y);
        $img->angle($angle);
        $img->line($len);

        ($x, $y) = $img->curPos();

        tree($x, $y, $len*$scale, $angle+35);
        tree($x, $y, $len*$scale, $angle-35);
}
