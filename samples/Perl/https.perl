use strict;
use LWP::UserAgent;

my $url = 'https://www.rosettacode.org';
my $response = LWP::UserAgent->new->get( $url );

$response->is_success or die "Failed to GET '$url': ", $response->status_line;

print $response->as_string;
