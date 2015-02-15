sub selection_sort
  {my @a = @_;
   foreach my $i (0 .. $#a - 1)
      {my $min = $i + 1;
       $a[$_] < $a[$min] and $min = $_ foreach $min .. $#a;
       $a[$i] > $a[$min] and @a[$i, $min] = @a[$min, $i];}
   return @a;}
