if( @ARGV != 3 ){
  printHelp();
}

  # translate to upper-case, remove anything else
map( (tr/a-z/A-Z/, s/[^A-Z]//g), @ARGV );

my $cipher_decipher = $ARGV[ 0 ];

if( $cipher_decipher !~ /ENC|DEC/ ){
  printHelp();  # user should say what to do
}

print "Key: " . (my $key = $ARGV[ 2 ]) . "\n";

if( $cipher_decipher =~ /ENC/ ){
  print "Plain-text: " . (my $plain = $ARGV[ 1 ]) . "\n";
  print "Encrypted: " . Vigenere( 1, $plain ) . "\n";
}elsif( $cipher_decipher =~ /DEC/ ){
  print "Cipher-text: " . (my $cipher = $ARGV[ 1 ]) . "\n";
  print "Decrypted: " . Vigenere( -1, $cipher ) . "\n";
}

sub printHelp{
  print "Usage:\n" .
        "Encrypting:\n perl cipher.pl ENC (plain text) (key)\n" .
        "Decrypting:\n perl cipher.pl DEC (cipher text) (key)\n";
  exit -1;
}

sub Vigenere{
  my ($direction, $text) = @_;
  for( my $count = 0; $count < length $text; $count ++ ){
    $key_offset = $direction * ord substr( $key, $count % length $key, 1);
    $char_offset = ord substr( $text, $count, 1 );
    $cipher .= chr 65 + ((($char_offset % 26) + ($key_offset % 26)) % 26);
      # 65 is the ASCII character code for 'A'
  }
  return $cipher;
}
