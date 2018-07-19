use strict;
use warnings;

my @B=qw(t h e q u i c k b r o w n f o x j u m p s o v e r t h e l a z y d o g);
print "@B\n";
my @C=cocktailSort(@B);
print "@C\n";
################### cocktailSort #####################
sub cocktailSort {   #( A : list of sortable items ) defined as:
  my @A = @_;
  my $swapped = 1;
  while ($swapped == 1) {
    $swapped = 0;
    for (my $i=0; $i<($#A-1); $i+=1) {

      if ($A[$i] gt $A[$i+1]) { # test whether the two
                            # elements are in the wrong
                            # order

         ($A[$i+1], $A[$i])=($A[$i], $A[$i+1]); # let the two elements
                                                # change places
        $swapped = 1;
      }
    }
    if ($swapped == 0) {
      # we can exit the outer loop here if no swaps occurred.
      print "no more swaps";
    }
    else {
    $swapped = 0;
    for (my $i=($#A-1); $i>0 ; $i-=1) {

      if($A[$i] gt $A[$i+1]) {

        ($A[$i+1], $A[$i])=($A[$i], $A[$i+1]);
        $swapped = 1;
      }
    }
    }
#  if no elements have been swapped,
#  then the list is sorted
  }
return (@A);
#end sub
}
