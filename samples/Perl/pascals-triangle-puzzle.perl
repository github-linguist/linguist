# set up triangle
my $rows = 5;
my @tri = map { [ map { {x=>0,z=>0,v=>0,rhs=>undef} } 1..$_ ] } 1..$rows;
$tri[0][0]{rhs} = 151;
$tri[2][0]{rhs} = 40;
$tri[4][0]{x} = 1;
$tri[4][1]{v} = 11;
$tri[4][2]{x} = 1;
$tri[4][2]{z} = 1;
$tri[4][3]{v} = 4;
$tri[4][4]{z} = 1;

# aggregate from bottom to top
for my $row ( reverse 0..@tri-2 ) {
    for my $col ( 0..@{$tri[$row]}-1 ){
        $tri[$row][$col]{$_} = $tri[$row+1][$col]{$_}+$tri[$row+1][$col+1]{$_} for 'x','z','v';
    }
}
# find equations
my @eqn;
for my $row ( @tri ) {
    for my $col ( @$row ){
        push @eqn, [ $$col{x}, $$col{z}, $$col{rhs}-$$col{v} ] if defined $$col{rhs};
    }
}
# print equations
print "Equations:\n";
print "  x +   z = y\n";
printf "%d x + %d z = %d\n", @$_ for @eqn;
# solve
my $f = $eqn[0][1] / $eqn[1][1];
$eqn[0][$_] -=  $f * $eqn[1][$_] for 0..2;
$f = $eqn[1][0] / $eqn[0][0];
$eqn[1][$_] -=  $f * $eqn[0][$_] for 0..2;
# print solution
print "Solution:\n";
my $x = $eqn[0][2]/$eqn[0][0];
my $z = $eqn[1][2]/$eqn[1][1];
my $y = $x+$z;
printf "x=%d, y=%d, z=%d\n", $x, $y, $z;
