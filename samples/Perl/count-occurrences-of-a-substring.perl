sub countSubstring {
  my $str = shift;
  my $sub = quotemeta(shift);
  my $count = () = $str =~ /$sub/g;
  return $count;
#  or return scalar( () = $str =~ /$sub/g );
}

print countSubstring("the three truths","th"), "\n"; # prints "3"
print countSubstring("ababababab","abab"), "\n"; # prints "2"
