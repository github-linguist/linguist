use LWP::Simple;

my $url = 'http://tycho.usno.navy.mil/cgi-bin/timer.pl';
get($url) =~ /<BR>(.+? UTC)/
    and print "$1\n";
