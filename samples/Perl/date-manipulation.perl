use DateTime;
use DateTime::Format::Strptime 'strptime';
use feature 'say';

my $input =  'March 7 2009 7:30pm EST';
$input    =~ s{EST}{America/New_York};

say strptime('%b %d %Y %I:%M%p %O', $input)
        ->add(hours => 12)
        ->set_time_zone('America/Edmonton')
        ->format_cldr('MMMM d yyyy h:mma zzz');
