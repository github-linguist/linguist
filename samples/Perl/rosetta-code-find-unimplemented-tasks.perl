use LWP::Simple 'get';

my $fmt = 'http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:%s&cmlimit=500&format=json';
my $lang = shift
    or die "No language given.\n";

sub urlencode
   {join '', map {sprintf '%%%02x', ord} split //, shift}
sub tasks
   {my $category = urlencode shift;
    my @tasks;
    my $json = get sprintf $fmt, $category;
    for (;;)
       {push @tasks, $json =~ /"title":"(.+?)"}/g;
        $json =~ /"cmcontinue":"(.+?)"}/ or last;
        $json = get sprintf $fmt . '&cmcontinue=%s',
            $category, urlencode $1;}
    return @tasks;}

my @all = tasks 'Programming_Tasks';
my %lang = map {$_, 1} tasks $lang
    or die "No such category.\n";
$lang{$_} or print "$_\n"
    foreach @all;
