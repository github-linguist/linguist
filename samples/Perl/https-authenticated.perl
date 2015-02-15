use LWP::UserAgent qw();
my $ua = LWP::UserAgent->new;
my $netloc = 'http://www.buddhism-dict.net/cgi-bin/xpr-dealt.pl:80';
$ua->credentials(
   $netloc,
   'CJK-E and Buddhist Dictionaries', # basic realm
   'guest',  # user
   '',       # empty pw
);
my $response = $ua->get($netloc);

use WWW::Mechanize qw();
my $mech = WWW::Mechanize->new;
$mech->get('https://login.yahoo.com/');
$mech->submit_form(with_fields => {
    login         => 'XXXXXX',
    passwd        => 'YYYYYY',
    '.persistent' => 'y',  # tick checkbox
});
