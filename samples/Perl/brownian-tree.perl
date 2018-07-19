sub PI() { atan2(1,1) * 4 }     # The, er, pi
sub STEP() { .5 }               # How far does the particle move each step. Affects
                                #       both speed and accuracy greatly
sub STOP_RADIUS() { 100 }       # When the tree reaches this far from center, end

# At each step, move this much towards center.  Bigger numbers help the speed because
# particles are less likely to wander off, but greatly affects tree shape.
# Should be between 0 and 1 ish.  Set to 0 for pain.
sub ATTRACT() { .2 }

my @particles = map([ map([], 0 .. 2 * STOP_RADIUS) ],  0 .. 2 * STOP_RADIUS);
push @{ $particles[STOP_RADIUS][STOP_RADIUS] }, [0, 0];

my $r_start = 3;
my $max_dist = 0;

sub dist2 {
        my ($dx, $dy) = ($_[0][0] - $_[1][0], $_[0][1] - $_[1][1]);
        $dx * $dx + $dy * $dy
}

sub move {
        my $p = shift;
        # moved too far, kill particle
        # return if dist2($p, [0, 0]) > 2 * $r_start * $r_start;
        $p->[0] += 2 * $r_start while $p->[0] < -$r_start;
        $p->[0] -= 2 * $r_start while $p->[0] >  $r_start;
        $p->[1] += 2 * $r_start while $p->[1] < -$r_start;
        $p->[1] -= 2 * $r_start while $p->[1] >  $r_start;

        my ($ix, $iy) = (int($p->[0]), int($p->[1]));
        my $dist = 2 * $r_start * $r_start;
        my $nearest;

        # see if the particle is close enough to stick to an exist one
        for ($ix - 1 .. $ix + 1) {
                my $idx = STOP_RADIUS + $_;
                next if $idx > 2 * STOP_RADIUS || $idx < 0;
                my $xs = $particles[ $idx ];
                for ($iy - 1 .. $iy + 1) {
                        my $idx = STOP_RADIUS + $_;
                        next if $idx > 2 * STOP_RADIUS || $idx < 0;
                        for (@{ $xs->[ $idx ] }) {
                                my $d = dist2($p, $_);
                                next if $d > 2;
                                next if $d > $dist;

                                $dist = $d;
                                $nearest = $_;
                        }
                }
        }

        # yes, found one
        if ($nearest) {
                my $displace = [ $p->[0] - $nearest->[0],
                                 $p->[1] - $nearest->[1] ];
                my $angle = atan2($displace->[1], $displace->[0]);
                $p->[0] = $nearest->[0] + cos($angle);
                $p->[1] = $nearest->[1] + sin($angle);

                push @{$particles[$ix + STOP_RADIUS][$iy + STOP_RADIUS]}, [ @$p ];
                $dist = sqrt dist2($p);

                if ($dist + 10 > $r_start && $r_start < STOP_RADIUS + 10) {
                        $r_start = $dist + 10
                }
                if (int($dist + 1) > $max_dist) {
                        $max_dist = int($dist + 1);
                        # write_eps();
                        # system('pstopnm -portrait -xborder 0 -yborder 0 test.eps 2> /dev/null');
                        # system('pnmtopng test.eps001.ppm 2>/dev/null > test.png');
                        return 3 if $max_dist >= STOP_RADIUS;
                }
                return 2;
        }

        # random walk
        my $angle = rand(2 * PI);
        $p->[0] += STEP * cos($angle);
        $p->[1] += STEP * sin($angle);

        # drag particle towards center by some distance
        my $nudge;
        if (sqrt(dist2($p, [0, 0])) > STOP_RADIUS + 1) {
                $nudge = 1;
        } else {
                $nudge = STEP * ATTRACT;
        }

        if ($nudge) {
                $angle = atan2($p->[1], $p->[0]);
                $p->[0] -= $nudge * cos($angle);
                $p->[1] -= $nudge * sin($angle);
        }

        return 1;
}

my $count;
PARTICLE: while (1) {
        my $a = rand(2 * PI);
        my $p = [ $r_start * cos($a), $r_start * sin($a) ];
        while ($_ = move($p)) {
                given ($_) {
                        when (1) { next }
                        when (2) { $count++; last; }
                        when (3) { last PARTICLE }
                        default  { last }
                }
        }
        print STDERR "$count $max_dist/@{[int($r_start)]}/@{[STOP_RADIUS]}\r" unless $count% 7;
}

sub write_eps {
        my $size = 128;
        my $p = $size / (STOP_RADIUS * 1.05);
        my $b = STOP_RADIUS * $p;
        if ($p < 1) {
                $size = STOP_RADIUS * 1.05;
                $b = STOP_RADIUS;
                $p = 1;
        }

        my $hp = $p / 2;

        open OUT, ">", "test.eps";

        # print EPS to standard out
        print OUT <<"HEAD";
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 @{[$size*2, $size*2]}
$size $size translate
/l{ rlineto }def
/c{ $hp 0 360 arc fill }def
-$size -$size moveto
$size 2 mul 0 l
0 $size 2 mul l
-$size 2 mul 0 l
closepath
0 setgray fill
0 setlinewidth .1 setgray 0 0 $b 0 360 arc stroke
.8 setgray /TimesRoman findfont 16 scalefont setfont
-$size 10 add $size -16 add moveto
(Step = @{[STEP]}  Attract = @{[ATTRACT]}) show
0 1 0 setrgbcolor newpath
HEAD

        for (@particles) {
                for (@$_) {
                        printf OUT "%.3g %.3g c ", map { $_ * $p } @$_ for @$_;
                }
        }
        print OUT "\n%%EOF";
        close OUT;
}

write_eps;
