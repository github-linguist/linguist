while (<>) { $cnt{lc chop}++ while length }
print "$_: ", $cnt{$_}//0, "\n" for 'a' .. 'z';
