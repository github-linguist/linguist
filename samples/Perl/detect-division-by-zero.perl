sub div_check
 {local $@;
  eval {$_[0] / $_[1]};
  $@ and $@ =~ /division by zero/;}
