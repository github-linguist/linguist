use List::Util 'sum';

my @condition = (
                 sub { 0 }, # dummy sub for index 0
                 sub { 13==@_ },
                 sub { 3==sum @_[7..12] },
                 sub { 2==sum @_[2,4,6,8,10,12] },
                 sub { $_[5] ? ($_[6] and $_[7]) : 1 },
                 sub { !$_[2] and !$_[3] and !$_[4] },
                 sub { 4==sum @_[1,3,5,7,9,11] },
                 sub { $_[2]==1-$_[3] },
                 sub { $_[7] ? ($_[5] and $_[6]) : 1 },
                 sub { 3==sum @_[1..6] },
                 sub { 2==sum @_[11..12] },
                 sub { 1==sum @_[7,8,9] },
                 sub { 4==sum @_[1..11] },
                );

sub miss {
  return grep { $condition[$_]->(@_) != $_[$_] } 1..12;
}

for (0..2**12-1) {
  my @truth = split //, sprintf "0%012b", $_;
  my @no = miss @truth;
  print "Solution: true statements are ", join( " ", grep { $truth[$_] } 1..12), "\n" if 0 == @no;
  print "1 miss (",$no[0],"): true statements are ", join( " ", grep { $truth[$_] } 1..12), "\n" if 1 == @no;
}
