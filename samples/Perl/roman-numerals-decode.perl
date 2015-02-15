use 5.10.0;

{
 my @trans = (
                [M  => 1000],     [CM => 900],
                [D  => 500],      [CD => 400],
                [C  => 100],      [XC => 90],
                [L  => 50],       [XL => 40],
                [X  => 10],       [IX => 9],
                [V  => 5],        [IV => 4],
                [I  => 1],
 );

  sub from_roman {
        my $r = shift;
        my $n = 0;
        foreach my $pair (@trans) {
          my ($k, $v) = @$pair;
          $n += $v while $r =~ s/^$k//i;
        }
        return $n
  }
}

say "$_: ", from_roman($_) for qw(MCMXC MDCLXVI MMVIII);
