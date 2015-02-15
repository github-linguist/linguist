sub encipher {
        my ($_, $k, $decode) = @_;
        $k = 26 - $k if $decode;
        join('', map(chr(((ord(uc $_) - 65 + $k) % 26) + 65), /([a-z])/gi));
}

my $msg = 'THE FIVE BOXING WIZARDS JUMP QUICKLY';
my $enc = encipher($msg, 10);
my $dec = encipher($enc, 10, 'decode');

print "msg: $msg\nenc: $enc\ndec: $dec\n";
