use File::Find qw(find);
my $dir     = '.';
my $pattern = 'foo';
find sub {print $File::Find::name if /$pattern/}, $dir;
