sub fs(&) {
        my $func = shift;
        sub { map $func->($_), @_ }
}

sub double($) { shift() * 2 }
sub square($) { shift() ** 2 }

my $fs_double = fs(\&double);
my $fs_square = fs(\&square);

my @s = 0 .. 3;
print "fs_double(@s): @{[ $fs_double->(@s) ]}\n";
print "fs_square(@s): @{[ $fs_square->(@s) ]}\n";

@s = (2, 4, 6, 8);
print "fs_double(@s): @{[ $fs_double->(@s) ]}\n";
print "fs_square(@s): @{[ $fs_square->(@s) ]}\n";
