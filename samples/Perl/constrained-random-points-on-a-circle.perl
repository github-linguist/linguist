my @points;
while (@points < 100) {
        my ($x, $y) = (int(rand(31))-15, int(rand(31)) - 15);
        my $r2 = $x*$x + $y*$y;
        next if $r2 < 100 || $r2 > 225;
        push @points, [$x, $y];
}

print << 'HEAD';
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox 0 0 400 400
200 200 translate 10 10 scale
0 setlinewidth
1 0 0 setrgbcolor
0 0 10 0 360 arc stroke
0 0 15 360 0 arcn stroke
0 setgray
/pt { .1 0 360 arc fill } def
HEAD

print "@$_ pt\n" for @points;
print "%%EOF";
