# RPN calculator
#
# Nigel Galloway April 2nd., 2012
#
$WSb = '(?:^|\s+)';
$WSa = '(?:\s+|$)';
$num = '([+-/]?(?:\.\d+|\d+(?:\.\d*)?))';
$op = '([-+*/^])';
sub myE {
  my $a = '('.$1.')'.$3.'('.$2.')';
  $a =~ s/\^/**/;
  return eval($a);
}
while (<>)  {
  while (s/$WSb$num\s+$num\s+$op$WSa/' '.myE().' '/e)  {}
  print ($_, "\n");
}
