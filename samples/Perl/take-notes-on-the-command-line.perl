my $file = 'notes.txt';
if ( @ARGV ) {
  open NOTES, '>>', $file or die "Can't append to file $file: $!";
  print NOTES scalar localtime, "\n\t@ARGV\n";
} else {
  open NOTES, '<', $file or die "Can't read file $file: $!";
  print <NOTES>;
}
close NOTES;
